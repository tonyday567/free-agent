;;; free-agent-bus-status.el --- live bus status, standalone -*- lexical-binding: t; -*-

;; Minimal standalone deliverable: `free-agent-bus-status' and its
;; transitive dependencies only, extracted from free-agent-bus-deck.el.
;;
;; One deliberate deviation from the deck: `free-agent-bus-status-mode'
;; derives from `special-mode' here instead of `free-agent-bus-view-mode'.
;; The view-mode parent contributes only generic-view keys (RET/t open a
;; thread, TAB cycles depth) and a revert binding that status overrides
;; anyway; the documented status keys (f, q) are installed by the mode
;; body itself and are unchanged.
;;
;; Views delivered: *free-agent-bus-status* only.  No composition, inbox,
;; log tail, READY, thread, board, reference-link, or evil integration.

;;; Code:

(require 'filenotify)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'time-date)

(defgroup free-agent-bus-deck nil
  "Emacs surface for the free-agent bus log."
  :group 'applications)

(defcustom free-agent-bus-executable "free-agent"
  "Executable used to read and scribe the bus.
Invoked as: free-agent bus SUBCOMMAND --root ROOT [...], body on stdin."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-root "~/bus"
  "Root directory for the bus. The log lives at ROOT/log.jsonl.
A root without a readable log is an explicit error, never an empty view."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-name "runner"
  "Name this deck posts as, and the identity every view reports."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-status-buffer-name "*free-agent-bus-status*"
  "Name of the live status buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-status-refresh-interval 5.0
  "Seconds between repaints of a live status buffer, as a positive number.
The log watch covers a new message; this timer covers the state a seat changes
without appending one — a cursor write, a heartbeat, a settlement, a failed
send — which the watch can never see.  One `bus status --live' runs per tick,
and the timer lasts exactly as long as the status buffer.

A value `run-with-timer' cannot repeat is refused where the timer is started,
because the failure is silent: zero and nil both yield a one-shot timer that
fires once and never again — the page freezes after a single repaint, which is
the defect this timer exists to fix — a string is taken the same way, and a
negative value is the one that would signal `Invalid repetition interval' inside
`run-with-timer'.  The guard is what keeps that from happening: all four are
refused here, the timer stays off, the message names the value, and the log
watch is the only repaint."
  :type 'number
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-status-label-width 72
  "Display width of a status row's activity, task, assignment, and card suffix."
  :type 'integer
  :group 'free-agent-bus-deck)

(defun free-agent-bus--expand-root ()
  "Expand `free-agent-bus-root' to an absolute path."
  (expand-file-name free-agent-bus-root))

(defun free-agent-bus--log-file ()
  "Return path to the bus log file."
  (expand-file-name "log.jsonl" (free-agent-bus--expand-root)))

(defun free-agent-bus--executable ()
  "Return the path to the CLI, or its name when it is on PATH."
  (if (file-name-absolute-p free-agent-bus-executable)
      free-agent-bus-executable
    (or (executable-find free-agent-bus-executable)
        free-agent-bus-executable)))

(defun free-agent-bus--check-root ()
  "Return the expanded root, or raise an explicit error.
A stale or missing root is an error, never an empty museum."
  (let ((root (free-agent-bus--expand-root)))
    (unless (file-directory-p root)
      (user-error "free-agent bus root does not exist: %s (set free-agent-bus-root)" root))
    (unless (file-readable-p (free-agent-bus--log-file))
      (user-error "free-agent bus has no readable log: %s (set free-agent-bus-root)"
                  (free-agent-bus--log-file)))
    root))

(defconst free-agent-bus--unlaunchable-status 127
  "Status reported when the CLI cannot be launched at all.
127 is the shell's own \"command not found\", and it is a number so every
caller's numeric test keeps working and `free-agent-bus--report-failure' has a
status to name.")

(defun free-agent-bus--unlaunchable (err)
  "A (STATUS . OUTPUT) pair for a CLI that could not be run, from ERR.
The executable is named because that is the runner's fix, and the signal's own
text is kept because it names the file that is missing."
  (cons free-agent-bus--unlaunchable-status
        (format "cannot run %s\n%s\n\nfree-agent-bus-executable is %S: an absolute path is used as given, any other name is looked up on PATH."
                (free-agent-bus--executable) (error-message-string err)
                free-agent-bus-executable)))

(defun free-agent-bus--cli (subcommand &rest args)
  "Run `free-agent bus SUBCOMMAND --root ROOT ARGS...'.
SUBCOMMAND is a command name or a command path such as (\"cursor\" \"set\").
Return (STATUS . OUTPUT).  Never signals on a non-zero exit: a failing
command stays visible to the runner instead of being hidden by an error.
A CLI that cannot be launched at all comes back the same way — status 127
carrying the reason — so a missing executable reaches the runner's error
surface rather than a signal from inside a refresh."
  (free-agent-bus--check-root)
  (let ((coding-system-for-read 'utf-8)
        (commands (if (listp subcommand) subcommand (list subcommand))))
    (with-temp-buffer
      (condition-case err
          (let ((status (apply #'call-process (free-agent-bus--executable) nil t nil
                               (append (list "bus") commands
                                       (list "--root" (free-agent-bus--expand-root)) args))))
            (cons status (string-trim-right (buffer-string))))
        (file-error (free-agent-bus--unlaunchable err))))))

(defun free-agent-bus--id-in-line (line)
  "Return the post id LINE carries, or nil.
Understands the log tail (`[id@ts]'), CLI views (`id from→to'), raw JSONL
(`{\"id\":N'), and text mentioning `id=<n>'.  A rendered post usually carries
`free-agent-bus-post-id' as a text property instead, which is cheaper and
exact; this is what reads a line that carries no property."
  (cond
   ((string-match "\\`\\[\\([0-9]+\\)@" line) (string-to-number (match-string 1 line)))
   ((string-match "\\`{\"id\":\\([0-9]+\\)" line) (string-to-number (match-string 1 line)))
   ((string-match "\\`\\([0-9]+\\)[ \t]" line) (string-to-number (match-string 1 line)))
   ((string-match "id=\\([0-9]+\\)" line) (string-to-number (match-string 1 line)))))

(defun free-agent-bus--field-at-point (property fallback)
  "Return the field PROPERTY carries at point, or FALLBACK's parse of the line.
A line the deck rendered carries its fields as text properties; a line it did
not carries none, so FALLBACK is called with the line as a string.  The property
is read at point and then at the beginning of the line, because a post's own
rendered text and its continuation lines differ in where the property starts."
  (or (get-text-property (point) property)
      (save-excursion
        (beginning-of-line)
        (or (get-text-property (point) property)
            (funcall fallback
                     (buffer-substring-no-properties (point) (line-end-position)))))))

(defun free-agent-bus--id-at-point ()
  "Return the post id on the current line, or nil.
A status row, and every post the deck renders itself, carries its id as a text
property; `free-agent-bus--id-in-line' reads it back from the text when the line
came from somewhere that does not set properties."
  (free-agent-bus--field-at-point 'free-agent-bus-post-id #'free-agent-bus--id-in-line))

(defun free-agent-bus--post-line-start (id)
  "Start of the line carrying post ID in the current buffer, or nil.
The property is what every rendered post carries, so this searches for the
property rather than for the text: a line re-rendered at another depth, or by
the CLI, still finds the same id.  A buffer where the id no longer renders
(an inbox after a mark-read) returns nil rather than walking past the last
property change."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (point-min))
          (found nil))
      (while (and (not found) pos (< pos (point-max)))
        (if (equal (get-text-property pos 'free-agent-bus-post-id) id)
            (setq found (save-excursion (goto-char pos) (line-beginning-position)))
          (setq pos (next-single-property-change pos 'free-agent-bus-post-id))))
      found)))

(defun free-agent-bus--position-record (pos)
  "Describe POS for a rebuild: its post, its column, its offset, its endness.
`free-agent-bus--id-at-point' decides the post — the rendered property first,
the line's own shape as the fallback — so a point that is not on a post records
no id and falls back to its offset."
  (save-excursion
    (goto-char pos)
    (list :id (free-agent-bus--id-at-point)
          :column (- pos (line-beginning-position))
          :offset pos
          :end (= pos (point-max)))))

(defun free-agent-bus--position-target (record)
  "The position RECORD should be restored to, in the buffer as it stands now.
The end of the buffer wins, so a tailing reader stays at the tail.  Then the
recorded post, so a rebuild that re-renders every line at a new depth keeps the
reader on the post they were following, at the column they were at.  Then the
raw offset, clamped, for a point that was not on a post at all."
  (or (and (plist-get record :end) (point-max))
      (when-let ((id (plist-get record :id)))
        (when-let ((start (free-agent-bus--post-line-start id)))
          (save-excursion
            (goto-char start)
            (goto-char (min (+ start (or (plist-get record :column) 0))
                            (line-end-position))))))
      (min (plist-get record :offset) (point-max))))

(defun free-agent-bus--remember-position ()
  "Remember where the reader is: point, and every window's point and start.
Each position is recorded as the post under it plus its offset and column, so a
render that changes every line's length can put the reader back on the same
post rather than on whatever now sits at the same offset."
  (list :point (free-agent-bus--position-record (point))
        :windows (mapcar (lambda (w)
                           (list w
                                 (free-agent-bus--position-record (window-point w))
                                 (free-agent-bus--position-record (window-start w))))
                         (get-buffer-window-list (current-buffer) nil t))))

(defun free-agent-bus--restore-position (state)
  "Put point, and each live window's point and start, back where STATE says.
A window start is restored without forcing, so point never becomes invisible on
a repaint."
  (goto-char (free-agent-bus--position-target (plist-get state :point)))
  (dolist (entry (plist-get state :windows))
    (let ((window (nth 0 entry)))
      (when (window-live-p window)
        (set-window-point window (free-agent-bus--position-target (nth 1 entry)))
        (set-window-start window (free-agent-bus--position-target (nth 2 entry)) t)))))

(defmacro free-agent-bus--preserving-position (&rest body)
  "Evaluate BODY, then put the reader back where they were.
A render erases the buffer and re-renders every line, and at another depth every
post renders a different length, so a raw offset lands in a different post:
`free-agent-bus--remember-position' records the post under the reader — at point
and in every window — and `free-agent-bus--restore-position' resolves it in the
new text, keeping the offset rule for a point that was not on a post.  Without
this a refresh, a ping, or a depth change throws the reader off the message they
were following."
  (declare (indent 0) (debug t))
  `(let ((state (free-agent-bus--remember-position)))
     ,@body
     (free-agent-bus--restore-position state)))

(defvar free-agent-bus-status--usage-cache (make-hash-table :test 'equal)
  "Latest completed Hermes usage readings, keyed by (ROOT . SEAT).")

(defvar free-agent-bus-status--usage-running (make-hash-table :test 'equal)
  "Running Hermes usage collectors, keyed by (ROOT . SEAT).")

(defvar-local free-agent-bus-status--model nil
  "Status model: (:root ROOT-ROW :rows VISIBLE-ROWS :latest NEWEST-POST).
The root summary is distinct from seat and raw CLI rows.  NEWEST-POST comes
from the same single log read that supplies the rows' assignment labels.")

(defvar-local free-agent-bus-status--file-watch nil
  "File-notify descriptor for this status buffer.")

(defvar-local free-agent-bus-status--refresh-timer nil
  "Debounce timer for status file notifications.")

(defvar-local free-agent-bus-status--periodic-timer nil
  "Periodic repaint timer for this status buffer.
Status repaints on the log watch and on this timer, so state that appends no
log message still reaches the page.")

(defvar-local free-agent-bus-status--processes nil
  "Usage collectors owned by this status buffer.")

(define-derived-mode free-agent-bus-status-mode special-mode "FAB-Status"
  "Major mode for message-driven live bus status."
  (setq-local revert-buffer-function
              (lambda (&rest _) (free-agent-bus-status-refresh)))
  (local-set-key (kbd "q") #'kill-current-buffer)
  (local-set-key (kbd "f") #'free-agent-bus-status-toggle-yellow)
  (add-hook 'kill-buffer-hook #'free-agent-bus-status-stop nil t))

(defun free-agent-bus-status--key (seat)
  "Return the usage-cache key for SEAT under the current bus root."
  (cons (free-agent-bus--expand-root) seat))

(defun free-agent-bus-status--first-useful-line (body)
  "Return the first nonempty, non-fence line of BODY, or nil."
  (let ((lines (split-string (or body "") "\n"))
        (found nil))
    (while (and lines (not found))
      (let ((line (string-trim (pop lines))))
        (unless (or (string-empty-p line)
                    (string-match-p "\\`[`'-]+\\'" line))
          (setq found line))))
    found))

(defun free-agent-bus-status--assignment-label (body)
  "Return a one-line assignment label for BODY, or nil for report traffic."
  (when-let ((line (free-agent-bus-status--first-useful-line body)))
    (let ((case-fold-search t)
          (line (if (string-match "^\\(?:[•*+-][ \t]+\\)?\\(?:@[^:]+:[ \t]*\\)?\\(.*\\)$" line)
                    (match-string 1 line)
                  line)))
      (unless (string-match-p
               "^\\(CLAIM\\|DONE\\|LINK\\|NOTE\\|REPORT\\|ACK\\|RECEIPT\\|✓\\|🟢\\|🟣\\|🟡\\|🔴\\|🔵\\|🚩\\)\\($\\|[ \t·]\\)\\|^I am seated\\b\\|^Standing by\\b"
               line)
        (string-join (split-string line "[ \t\r\n]+" t) " ")))))

(defun free-agent-bus-status--log-model (seats)
  "Read the log once for SEATS' latest assignments and the newest post.
Return (ASSIGNMENTS . NEWEST), where ASSIGNMENTS maps a seat to (LABEL . ID)
and NEWEST is the last parseable post object, or nil for an empty log."
  (let ((assignments (make-hash-table :test 'equal))
        (newest nil))
    (dolist (line (split-string (free-agent-bus-log--read-file) "\n" t))
      (when-let ((obj (ignore-errors (json-read-from-string line))))
        (setq newest obj)
        (let ((to (append (alist-get 'to obj) nil))
              (body (alist-get 'body obj))
              (id (alist-get 'id obj)))
          (when-let ((label (free-agent-bus-status--assignment-label body)))
            (dolist (seat seats)
              (when (member seat to)
                (puthash seat (cons label id) assignments)))))))
    (cons assignments newest)))

(defun free-agent-bus-status--parse-row (line)
  "Parse one `bus status --live' LINE as a root, seat, or raw CLI row."
  (cond
   ((string-match
     "^\\([🟢🟡⚪🟣]+\\) \\([^ ]+\\) — \\([^;]+\\).*; cursor \\([^;]+\\); \\(?:last post\\|no posts\\).*$"
     line)
    (list :kind 'seat
          :text line
          :dot (match-string 1 line)
          :seat (match-string 2 line)
          :freshness (match-string 3 line)
          :cursor (match-string 4 line)
          :label nil :task-id nil
          :activity nil :task nil :card nil))
   ((string-match "^\\([🟢🟡⚪]+\\) " line)
    (list :kind 'root :text line :dot (match-string 1 line)))
   (t
    (list :kind 'raw :text line :seat nil :label nil :task-id nil
          :activity nil :task nil :card nil))))

(defun free-agent-bus-status--session (seat freshness)
  "Classify SEAT's Hermes session without waiting on a usage process."
  (if (string= freshness "no heartbeat")
      'n/a
    (let ((path (expand-file-name
                 (format ".sessions/%s.sid" seat)
                 (free-agent-bus--expand-root))))
      (cond
       ((not (file-exists-p path)) 'first-run)
       ((not (file-readable-p path)) 'unavailable)
       (t
        (let ((sid (condition-case nil
                       (with-temp-buffer
                         (insert-file-contents path nil 0 256)
                         (string-trim (buffer-string)))
                     (error nil))))
          (cond
           ((null sid) 'unavailable)
           ((string-empty-p sid) 'first-run)
           ((string-prefix-p "local-" sid) (cons 'hermes path))
           (t 'n/a))))))))

(defun free-agent-bus-status--usage-text (seat freshness)
  "Return SEAT's cached usage display, without starting a collector."
  (let ((session (free-agent-bus-status--session seat freshness)))
    (if (not (eq (car-safe session) 'hermes))
        (pcase session
          ('first-run "first run")
          ('n/a "n/a")
          (_ "unavailable"))
      (let ((cached (gethash (free-agent-bus-status--key seat)
                             free-agent-bus-status--usage-cache 'missing)))
        (pcase cached
          ('missing "pending")
          ('unavailable "unavailable")
          ((pred integerp) (format "%dk" (/ (+ cached 500) 1000)))
          (_ "unavailable"))))))

(defun free-agent-bus-status--freshness-text (freshness)
  "Reduce CLI freshness to the short age used by status rows."
  (if (string-match "^\\(working\\|idle\\) \\([^ ]+\\) ago$" freshness)
      (match-string 2 freshness)
    (if (string= freshness "no heartbeat") "—" freshness)))

(defun free-agent-bus-status--age-text (seconds)
  "Render nonnegative SECONDS as a short relative age."
  (cond
   ((< seconds 60) (format "%ds ago" seconds))
   ((< seconds 3600) (format "%dm ago" (/ seconds 60)))
   ((< seconds 86400) (format "%dh ago" (/ seconds 3600)))
   (t (format "%dd ago" (/ seconds 86400)))))

(defun free-agent-bus-status--timestamp-text (timestamp now)
  "Render TIMESTAMP as local time and relative age at NOW.
A timestamp later than NOW renders as `future timestamp' instead of an age."
  (let* ((decoded (parse-time-string timestamp))
         (time (progn
                 ;; `parse-time-string' leaves the zone nil for the log's
                 ;; "YYYY-MM-DD HH:MM:SS UTC" spelling; nil means local to
                 ;; `encode-time', so supply the zone the log explicitly names.
                 (when (string-suffix-p " UTC" timestamp)
                   (setf (nth 8 decoded) 0))
                 (encode-time decoded)))
         (seconds (floor (float-time (time-subtract now time)))))
    (if (< seconds 0)
        "future timestamp"
      (format "%s (%s)"
              (string-trim (format-time-string "%l:%M%P" time))
              (free-agent-bus-status--age-text seconds)))))

(defun free-agent-bus-status--header-text (root latest watch now)
  "Render the compact header from ROOT row and LATEST post at NOW.
WATCH controls only the unavailable warning and the help-echo detail; normal
watcher implementation prose stays out of the header text.  Return nil when
the CLI supplied no root summary, leaving its raw failure rows visible."
  (when root
    (let* ((fields (list (concat (plist-get root :dot) " "
                                 (abbreviate-file-name (free-agent-bus--expand-root)))
                         (format "posting as %s" free-agent-bus-name)
                         (if latest
                             (format "last %s" (alist-get 'id latest))
                           "no posts")))
           (fields (if latest
                       (append fields
                               (list (free-agent-bus-status--timestamp-text
                                      (alist-get 'ts latest) now)))
                     fields))
           (header (string-join fields " · ")))
      (propertize
       (concat header (unless watch " · watch unavailable"))
       'face 'bold
       'help-echo (if watch
                      "file-notify watch active; periodic refresh enabled"
                    "file-notify unavailable; periodic refresh remains active")))))

(defvar-local free-agent-bus-status-hide-yellow nil
  "When non-nil, this status page drops rows whose dot is yellow.
Yellow marks an idle seat parked past an hour; hiding keeps the busy and
ready rows in view.  Toggle with `free-agent-bus-status-toggle-yellow'.")

(defun free-agent-bus-status-toggle-yellow ()
  "Toggle hiding yellow rows (seats idle over an hour) in this status page."
  (interactive)
  (setq-local free-agent-bus-status-hide-yellow
              (not free-agent-bus-status-hide-yellow))
  (free-agent-bus-status-refresh)
  (message "status: %s yellow rows"
           (if free-agent-bus-status-hide-yellow "hiding" "showing")))

(defun free-agent-bus-status--visible-row-p (row)
  "Whether ROW carries seat state or is a raw CLI row that must remain visible.
With `free-agent-bus-status-hide-yellow' set, yellow seat rows (idle over an
hour) drop out of the model; raw and root rows always stay."
  (or (not (eq (plist-get row :kind) 'seat))
      (and (not (and free-agent-bus-status-hide-yellow
                     (equal (plist-get row :dot) "🟡")))
           (not (and (string= (plist-get row :freshness) "no heartbeat")
                     (string= (plist-get row :cursor) "—"))))))

(defun free-agent-bus-status--row-suffix (row)
  "Render ROW's activity, task or assignment fallback, and card metadata.
Only nonempty metadata values join the suffix.  Task replaces the assignment
label; with neither, the existing `—' remains.  With no metadata the text is
the old assignment-label rendering, including its display-width bound."
  (let* ((present (lambda (value)
                    (and (stringp value) (not (string-empty-p value)) value)))
         (activity (funcall present (plist-get row :activity)))
         (task (or (funcall present (plist-get row :task))
                   (plist-get row :label)
                   "—"))
         (card (funcall present (plist-get row :card))))
    (truncate-string-to-width
     (string-join (delq nil (list activity task card)) " ⟜ ")
     free-agent-bus-status-label-width nil nil t)))

(defun free-agent-bus-status--row-text (row)
  "Render one parsed status ROW with usage and assignment context."
  (if-let ((seat (plist-get row :seat)))
      (let* ((id (plist-get row :task-id))
             (line (string-join
                    (list (concat (plist-get row :dot) " " seat)
                          (free-agent-bus-status--usage-text
                           seat (plist-get row :freshness))
                          (free-agent-bus-status--freshness-text
                           (plist-get row :freshness))
                          (plist-get row :cursor)
                          (free-agent-bus-status--row-suffix row))
                    " ⟜ ")))
        (if id
            (propertize line 'free-agent-bus-post-id id
                        'help-echo (format "RET: open assignment thread %s" id))
          line))
    (plist-get row :text)))

(defun free-agent-bus-status--render ()
  "Render the current status model, skipping an unchanged repaint."
  (let* ((model free-agent-bus-status--model)
         (watch free-agent-bus-status--file-watch)
         (contents
          (with-temp-buffer
            (when-let ((header (free-agent-bus-status--header-text
                                (plist-get model :root)
                                (plist-get model :latest)
                                watch (current-time))))
              (insert header "\n\n"))
            (dolist (row (plist-get model :rows))
              (insert (free-agent-bus-status--row-text row) "\n"))
            (buffer-string))))
    (unless (equal-including-properties contents (buffer-string))
      (let ((inhibit-read-only t))
        (free-agent-bus--preserving-position
          (erase-buffer)
          (insert contents))
        (set-buffer-modified-p nil)))))

(defun free-agent-bus-status--usage-sentinel (proc _event)
  "Cache PROC's completed usage reading and repaint only this status."
  (when (memq (process-status proc) '(exit signal closed failed))
    (let* ((key (process-get proc 'free-agent-bus-key))
           (buf (process-get proc 'free-agent-bus-buffer))
           (current (eq (gethash key free-agent-bus-status--usage-running) proc)))
      (when current (remhash key free-agent-bus-status--usage-running))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local free-agent-bus-status--processes
                      (delq proc free-agent-bus-status--processes))))
      (when (and current (not (process-get proc 'free-agent-bus-cancelled)))
        (let ((tokens (and (zerop (process-exit-status proc))
                           (string-match "prompt_tokens=\\([0-9]+\\)"
                                         (or (process-get proc 'free-agent-bus-output) ""))
                           (string-to-number
                            (match-string 1 (process-get proc 'free-agent-bus-output))))))
          (puthash key (or tokens 'unavailable) free-agent-bus-status--usage-cache))
        (when (buffer-live-p buf)
          (with-current-buffer buf (free-agent-bus-status--render)))))))

(defun free-agent-bus-status--collect (seat path)
  "Start one asynchronous Hermes usage collector for SEAT at PATH."
  (let* ((key (free-agent-bus-status--key seat))
         (buf (current-buffer)))
    (unless (gethash key free-agent-bus-status--usage-running)
      (let ((proc (make-process
                   :name (format "free-agent-usage-%s" seat)
                   :buffer nil
                   :command (list (free-agent-bus--executable) "usage" "hermes" path)
                   :noquery t
                   :filter (lambda (p text)
                             (process-put p 'free-agent-bus-output
                                          (concat (process-get p 'free-agent-bus-output) text)))
                   :sentinel #'free-agent-bus-status--usage-sentinel)))
        (process-put proc 'free-agent-bus-key key)
        (process-put proc 'free-agent-bus-buffer buf)
        (puthash key proc free-agent-bus-status--usage-running)
        (push proc free-agent-bus-status--processes)))))

(defun free-agent-bus-status--collect-usage ()
  "Collect fresh usage for visible Hermes seats without overlapping collectors."
  (dolist (row (plist-get free-agent-bus-status--model :rows))
    (when-let ((seat (plist-get row :seat)))
      (let ((session (free-agent-bus-status--session
                      seat (plist-get row :freshness))))
        (when (eq (car-safe session) 'hermes)
          (free-agent-bus-status--collect seat (cdr session)))))))

(defun free-agent-bus-status--refresh-model ()
  "Re-read the live rows and repaint.
Usage stays asynchronous: the caller decides whether to start collectors, so a
periodic repaint does not spawn a usage process every tick.  One log read
supplies both assignment labels and the newest post used by the header."
  (free-agent-bus--check-root)
  (let* ((result (free-agent-bus--cli "status" "--live" "--depth" "1"))
         (status (car result))
         (output (cdr result))
         (parsed (if (string-empty-p output)
                     (list (list :kind 'raw :text "(no rows)" :seat nil
                                 :label nil :task-id nil
                                 :activity nil :task nil :card nil))
                   (mapcar #'free-agent-bus-status--parse-row
                           (split-string output "\n" t))))
         (root (seq-find (lambda (row) (eq (plist-get row :kind) 'root)) parsed))
         (rows (seq-remove (lambda (row) (eq (plist-get row :kind) 'root)) parsed))
         (seats (delq nil (mapcar (lambda (row)
                                    (and (plist-get row :seat)
                                         (not (string= (plist-get row :freshness)
                                                       "no heartbeat"))
                                         (plist-get row :seat)))
                                  rows)))
         (log-model (free-agent-bus-status--log-model seats))
         (assignments (car log-model)))
    (dolist (row rows)
      (when-let ((seat (plist-get row :seat)))
        (when-let ((assignment (gethash seat assignments)))
          (plist-put row :label (car assignment))
          (plist-put row :task-id (cdr assignment)))))
    (setq-local free-agent-bus-status--model
                (list :root root
                      :rows (append (seq-filter #'free-agent-bus-status--visible-row-p rows)
                                    (unless (zerop status)
                                      (list (list :kind 'raw
                                                  :text (format "⛔ free-agent bus status exited %d" status)
                                                  :seat nil :label nil :task-id nil
                                                  :activity nil :task nil :card nil))))
                      :latest (cdr log-model)))
    (free-agent-bus-status--render)))

(defun free-agent-bus-status-refresh ()
  "Refresh status and assignment labels now; usage remains asynchronous."
  (interactive)
  (free-agent-bus-status--refresh-model)
  (free-agent-bus-status--collect-usage))

(defun free-agent-bus--cancel-timer (timer-var &optional buffer)
  "Cancel the timer TIMER-VAR holds, and leave the variable nil.
With BUFFER the variable is that buffer's own, so only its timer is touched;
without it the variable is the global one.  Safe on a variable that holds nil or
a timer that has already fired."
  (let ((timer (if buffer
                   (buffer-local-value timer-var buffer)
                 (symbol-value timer-var))))
    (when (timerp timer)
      (cancel-timer timer))
    (if buffer
        (with-current-buffer buffer
          (set (make-local-variable timer-var) nil))
      (set timer-var nil))))

(defun free-agent-bus--arm-timer (timer-var delay repeat function &optional buffer)
  "Arm a timer in TIMER-VAR, cancelling whatever that variable already held.
DELAY and REPEAT go to `run-with-timer' unchanged, so the log's polling fallback
starts at 0 and the status repaint waits its interval.  With BUFFER the timer
belongs to that buffer: FUNCTION runs there and only while the buffer is alive,
and the variable is set buffer-locally.  Returns the timer, or nil when REPEAT
cannot repeat — the variable is left nil then, and the caller says why."
  (free-agent-bus--cancel-timer timer-var buffer)
  (when (free-agent-bus--interval-p repeat)
    (let ((timer (run-with-timer
                  delay repeat
                  (if buffer
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (funcall function))))
                    function))))
      (if buffer
          (with-current-buffer buffer
            (set (make-local-variable timer-var) timer))
        (set timer-var timer))
      timer)))

(defun free-agent-bus--debounce (timer-var delay function &optional buffer)
  "Coalesce a burst of events into one call to FUNCTION, DELAY seconds later.
TIMER-VAR names the variable that holds the pending timer: it is cancelled
first, and cleared when the timer fires, so it never keeps a dead handle.  With
BUFFER the timer belongs to that buffer — it is set buffer-locally there,
FUNCTION runs with that buffer current, and a killed buffer drops the call.
Returns the armed timer."
  (free-agent-bus--cancel-timer timer-var buffer)
  (let ((callback
         (lambda ()
           (if buffer
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (set (make-local-variable timer-var) nil)
                   (funcall function)))
             (set timer-var nil)
             (funcall function)))))
    (if buffer
        (with-current-buffer buffer
          (set (make-local-variable timer-var)
               (run-with-timer delay nil callback)))
      (set timer-var (run-with-timer delay nil callback)))))

(defun free-agent-bus-status--refresh-soon ()
  "Coalesce file-notification events into one status refresh."
  (free-agent-bus--debounce 'free-agent-bus-status--refresh-timer 0.05
                            (lambda () (ignore-errors (free-agent-bus-status-refresh)))
                            (current-buffer)))

(defun free-agent-bus--interval-p (value)
  "Whether VALUE can repeat a timer: a positive number of seconds."
  (and (numberp value) (> value 0)))

(defun free-agent-bus-status--start-periodic ()
  "Repaint this status buffer on a timer, when the interval can bear one.
The log watch fires on a new message; this covers the state that appends no
log message at all.  The timer is owned by the buffer, not the window, and is
cancelled by `free-agent-bus-status-stop' when the buffer is killed.

`free-agent-bus-status-refresh-interval' must be a positive number.  Measured:
zero and nil both arm a one-shot timer that fires once and never repeats — the
page freezes after a single repaint, which is the defect this timer exists to
fix — a string is taken the same way, and a negative value would signal `Invalid
repetition interval' inside `run-with-timer'.  The guard is what keeps that from
happening: all four are refused here, the timer stays off, the message names the
value, and the log watch is the only repaint.  Returns the timer, or nil when
none was started."
  (or (free-agent-bus--arm-timer
       'free-agent-bus-status--periodic-timer
       free-agent-bus-status-refresh-interval
       free-agent-bus-status-refresh-interval
       (lambda () (ignore-errors (free-agent-bus-status--refresh-model)))
       (current-buffer))
      (progn
        (message "free-agent status: repaint interval %S is not a positive number; the timer is not started"
                 free-agent-bus-status-refresh-interval)
        nil)))

(defun free-agent-bus-status--stop-periodic ()
  "Cancel this status buffer's periodic repaint timer."
  (free-agent-bus--cancel-timer 'free-agent-bus-status--periodic-timer (current-buffer)))

(defun free-agent-bus-status-stop-watch ()
  "Remove the status log watch and its pending debounce timer."
  (free-agent-bus--cancel-timer 'free-agent-bus-status--refresh-timer (current-buffer))
  (when free-agent-bus-status--file-watch
    (file-notify-rm-watch free-agent-bus-status--file-watch)
    (setq-local free-agent-bus-status--file-watch nil)))

(defun free-agent-bus--watch-log-file (on-event &optional fallback)
  "Call ON-EVENT on every change to the log file, and return the watch descriptor.
ON-EVENT runs with this buffer current, and only while it is alive, so a watch
whose buffer has gone drops the event rather than erroring inside a timer or a
file-notify callback.  When file-notify cannot be used, FALLBACK is called if it
is given — the log's starts its polling timer, the status pane's says so — and
nil is returned either way, which is what an empty watch variable should hold."
  (let ((buffer (current-buffer)))
    (condition-case nil
        (file-notify-add-watch (free-agent-bus--log-file) '(change)
                               (lambda (_event)
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (funcall on-event)))))
      (error
       (when fallback (funcall fallback))
       nil))))

(defun free-agent-bus-status-start-watch ()
  "Refresh status on log messages and on a periodic repaint.
The log watch covers a new message; `free-agent-bus-status-refresh-interval'
covers the state that appends no log message.  file-notify is preferred but
never required."
  (free-agent-bus-status-stop-watch)
  (setq-local free-agent-bus-status--file-watch
              (free-agent-bus--watch-log-file
               #'free-agent-bus-status--refresh-soon
               (lambda ()
                 (message "free-agent status: file-notify unavailable; repainting on a timer")))))

(defun free-agent-bus-status-stop ()
  "Remove the status watch, stop its repaint timer, and cancel its collectors."
  (free-agent-bus-status-stop-watch)
  (free-agent-bus-status--stop-periodic)
  (dolist (proc free-agent-bus-status--processes)
    (process-put proc 'free-agent-bus-cancelled t)
    (let ((key (process-get proc 'free-agent-bus-key)))
      (when (eq (gethash key free-agent-bus-status--usage-running) proc)
        (remhash key free-agent-bus-status--usage-running)))
    (when (process-live-p proc) (delete-process proc)))
  (setq-local free-agent-bus-status--processes nil))

(defun free-agent-bus-status--prepare ()
  "Prepare the current buffer as a fresh message-driven status view."
  (unless (derived-mode-p 'free-agent-bus-status-mode)
    (free-agent-bus-status-mode))
  (free-agent-bus-status-start-watch)
  (free-agent-bus-status--start-periodic)
  (free-agent-bus-status-refresh))

;;;###autoload
(defun free-agent-bus-status ()
  "Open live status with prompt usage and latest-assignment labels.
Log messages refresh the view, and a periodic repaint covers the state that
appends no log message, so the page does not go stale between posts.  Hermes
usage collection is asynchronous and cached.  No cursor is written and no
model is invoked."
  (interactive)
  (free-agent-bus--check-root)
  (let ((buf (get-buffer-create free-agent-bus-status-buffer-name)))
    (with-current-buffer buf (free-agent-bus-status--prepare))
    (pop-to-buffer buf)))

(defun free-agent-bus-log--read-file (&optional start end)
  "Read the bus log file as UTF-8 text, optionally from START to END.
START and END are byte offsets, allowing an append to be read without
re-reading the log prefix."
  (with-temp-buffer
    (insert-file-contents-literally (free-agent-bus--log-file) nil start end)
    (set-buffer-file-coding-system 'utf-8)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (buffer-string)))

(provide 'free-agent-bus-status)
;;; free-agent-bus-status.el ends here
