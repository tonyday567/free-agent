;;; free-agent-bus-deck.el --- free-agent bus runner deck -*- lexical-binding: t; -*-

;; An Emacs surface for the free-agent bus log (generation two, root ~/bus).
;;
;; The log is the record; the `free-agent bus' CLI is the contract. This file
;; renders the record and submits explicit acts through that CLI. It is not a
;; second bus, task store, cursor owner, or agent loop.
;;
;; Views:
;;   *free-agent-bus-status*  live seats, freshness        (bus status --live)
;;   *free-agent-bus-ready*   open unclaimed TASKs         (bus ready)
;;   *free-agent-bus-thread*  ancestry of one post         (bus read --thread ID)
;;   *free-agent-bus-log*     tail of the log, depth-aware
;;   *free-agent-bus-inbox*   unread mail for this name    (bus inbox NAME)
;;   *free-agent-bus-post*    composition buffer           (bus say)
;;
;; Cascade depth is explicit everywhere: 0 dot, 1 one-line excerpt, 2 verbatim.
;; No view advances an agent delivery cursor and no view invokes a model.
;;
;; The human: `free-agent-bus-name' is a durable bus name like any other, and
;; its unread state is `.cursor-NAME' in the root, read through `bus inbox'.
;; The `.human-NAME' marker that `bus attach' writes is evidence that the name
;; is a person, not a seat: the inbox header prints it, and nothing in this
;; deck claims privilege from it. The inbox browser never writes a cursor; only
;; the explicit mark-read action does, through `bus inbox --mark-read'.
;;
;; Audience: a post with `to: []' is public and is always shown. There is no
;; broadcast token: the retired `"all"' convention is gone from composition and
;; from the filter, and `free-agent-bus-post-to' has no implicit default.
;;
;; Keys (Doom leader; also bound inside the buffer maps):
;;   SPC y RET / SPC y SPC   send region (or whole post buffer)
;;   SPC y m                 mark region: a support pin — buffer, position,
;;                           quoted text — drafted with an explicit SPC y a/t
;;                           choice kept for one mark, else the respondent
;;                           resolved (no region quotes the line; no target
;;                           clears and the header says so; C-u sends instantly)
;;   SPC y p                 composition buffer
;;   SPC y l                 log tail
;;   SPC y s                 live status
;;   SPC y r                 READY tasks
;;   SPC y t                 thread at point (or prompt for a post id)
;;   SPC y i                 inbox: unread mail for this name
;;   SPC y f                 set the audience filter (empty = show everything)
;;   SPC y b                 board file — retired: its default path is gone, so
;;                           the row is marked, not removed (the runner rules)
;;   SPC y d                 deck: status + log + compose in one frame
;;   SPC y q                 close deck buffers
;; In *free-agent-bus-log* and the view buffers:
;;   RET  open the thread of the post at point      TAB  cycle cascade depth
;;   t/f  timestamps / filter (log only)            q  quit the window
;; In *free-agent-bus-inbox*:
;;   m    mark read (bus inbox NAME --mark-read)  r  reply into the thread at
;;   RET/t  thread at point                       point (composition buffer)
;;
;; evil's state maps outrank a plain buffer-local major-mode map, so every key
;; above is installed a second time through evil's auxiliary keymaps in
;; `free-agent-bus--evil-bind-keys'.  `g' is not a deck key: it stays evil's
;; goto prefix in every deck buffer, and refreshing is automatic instead — the
;; log tails on its own watch, and status repaints on a timer.
;;
;; Marks: a post the attached human wrote renders in
;; `free-agent-bus-post-mine-face', and a post with ancestry renders
;; `↩ROOT' in `free-agent-bus-thread-face'.  Both read the log's own fields,
;; `from' and `thread'; a conversation is never inferred from timestamps or
;; sender names.  RET opens the conversation, so the root's own label does not
;; need repeating on every reply.
;; The log tail indexes posts as it renders them; a view or the inbox, which has
;; no tail, reads the log once per refresh (`free-agent-bus-log--read-index')
;; and adds the same mark to its own lines.

;;; Code:

(require 'filenotify)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'time-date)

;;; Variables

(defgroup free-agent-bus-deck nil
  "Emacs surface for the free-agent bus log."
  :group 'applications)

(defface free-agent-bus-post-mine-face
  '((t :inherit (bold font-lock-function-name-face)))
  "Face on the summary line of a post `free-agent-bus-name' wrote.
This is the only thing that has to distinguish the attached human's own
posts from the fleet's in a tail where both are just lines of text."
  :group 'free-agent-bus-deck)

(defface free-agent-bus-thread-face
  '((t :inherit shadow))
  "Face on a post's ancestry in the log tail: its thread root and that
root's own label.  Quiet by default — the ancestry is context for the post
beside it, not a second copy of the conversation."
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-excerpt-width 72
  "Display width of a post's first-line excerpt at cascade depth 1.
The full body remains available at depth 2 and through RET on the post."
  :type 'integer
  :group 'free-agent-bus-deck)

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

(defcustom free-agent-bus-post-to nil
  "Explicit recipient list for the next post, or nil when none is set.
There is no broadcast default: `free-agent-bus-send' refuses a post with no
recipients and no `free-agent-bus-post-public'."
  :type '(repeat string)
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-post-public nil
  "When non-nil, the next post is public (`to: []') — addressed to everyone."
  :type 'boolean
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-post-thread nil
  "Optional ancestry for the next post: a post id whose thread it answers."
  :type '(choice (const :tag "none" nil) integer)
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-post-buffer-name "*free-agent-bus-post*"
  "Name of the composition buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-buffer-name "*free-agent-bus-log*"
  "Name of the log tail buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-status-buffer-name "*free-agent-bus-status*"
  "Name of the live status buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-ready-buffer-name "*free-agent-bus-ready*"
  "Name of the READY task buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-thread-buffer-name "*free-agent-bus-thread*"
  "Name of the thread view buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-inbox-buffer-name "*free-agent-bus-inbox*"
  "Name of the inbox buffer: unread mail for `free-agent-bus-name'."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-inbox-depth 1
  "Initial cascade depth of the inbox: 0 dot, 1 one-line excerpt, 2 verbatim.
Depth 1 is the reading default; TAB reaches the other depths."
  :type '(choice (const 0) (const 1) (const 2))
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-board-file "~/coffee/loom/board.md"
  "Path to the board markdown file, opened as a regular file.
The loom board card has since been retired; set this to whatever card now
holds the runner's board, or use `SPC y r' for the READY view instead."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-refresh-interval 1.0
  "Seconds between log refreshes when file-notify is unavailable.
A value `run-with-timer' cannot repeat is refused where that fallback timer is
armed, because the failure is silent: the tail would refresh once and then never
again.  See `free-agent-bus-status-refresh-interval' for the measurement."
  :type 'number'
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-auto-refresh-interval 5.0
  "Seconds between re-runs of visible READY and thread CLI views.
Status is message-driven instead.  The CLI reads the log and writes no cursor;
a paused view costs nothing.

A value `run-with-timer' cannot repeat is refused where the timer is started,
for the same reason as `free-agent-bus-status-refresh-interval': the views would
refresh once and then quietly stop being fresh."
  :type 'number'
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

(defcustom free-agent-bus-log-show-timestamps t
  "Whether to show post ids and timestamps in the log tail."
  :type 'boolean
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-depth 1
  "Cascade depth for the log tail: 0 dot, 1 one-line excerpt, 2 verbatim."
  :type '(choice (const 0) (const 1) (const 2))
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-filter-choices nil
  "Names offered first by `free-agent-bus-log-set-filter' completion.
Empty by default on purpose: a roster of seats belongs to one root and goes
stale — the default this replaced named seven seats, none of them on the root
the deck was reading — while the log's own senders are the truth and the prompt
takes any name typed in full.  Set this to the names you filter by.
Public posts (`to: []') are shown whatever the filter."
  :type '(repeat string)
  :group 'free-agent-bus-deck)

(defvar-local free-agent-bus-log-filter nil
  "Audience filter for this log buffer: a list of names, or nil.
nil shows every post.  A filter shows public posts and any post whose `to'
field names one of the filter names.  The retired `\"all\"' token is not
special: it is a name like any other.")

;;; Core helpers

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

(defun free-agent-bus--cli-stdin (input subcommand &rest args)
  "Run the CLI with INPUT on stdin.  Return (STATUS . OUTPUT).
OUTPUT is what the CLI wrote and not the input echoed back:
`call-process-region' leaves the region in place, so the reply begins where the
region ended, and taking the whole buffer would hand the caller its own text
prefixed to the reply — which is what a receipt line then prints.  Like
`free-agent-bus--cli', an executable that cannot be launched is a status rather
than a signal, so a send that cannot run says so and keeps the draft."
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert input)
      (let ((reply-from (point-max)))
        (condition-case err
            (let ((status (apply #'call-process-region (point-min) reply-from
                                 (free-agent-bus--executable) nil t nil
                                 "bus" subcommand "--root" (free-agent-bus--expand-root) args)))
              (cons status (string-trim-right (buffer-substring reply-from (point-max)))))
          (file-error (free-agent-bus--unlaunchable err)))))))

(defun free-agent-bus--context-line ()
  "Identity and freshness line shared by every view."
  (format "root %s · posting as %s · %s"
          (free-agent-bus--expand-root)
          free-agent-bus-name
          (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun free-agent-bus--human-marker-file ()
  "Path to the attached-human marker for `free-agent-bus-name'."
  (expand-file-name (format ".human-%s" free-agent-bus-name)
                    (free-agent-bus--expand-root)))

(defun free-agent-bus--attached-line ()
  "The explicit attached-human line: who this deck is, and where.
Reads the `.human-NAME' marker that `bus attach' writes, so detachment is
never ambiguous.  The marker is evidence, never authority: this line reports
it and no act in this deck depends on it."
  (let* ((root (free-agent-bus--expand-root))
         (file (free-agent-bus--human-marker-file))
         (marker (and (file-readable-p file)
                      (ignore-errors (json-read-file file)))))
    (cond
     (marker
      (format "attached as %s on %s (cursor %s, since %s)"
              (or (alist-get 'name marker) free-agent-bus-name)
              (or (alist-get 'root marker) root)
              (or (alist-get 'cursor marker) "?")
              (or (alist-get 'attached_at marker) "?")))
     ((file-readable-p file)
      (format "attached as %s on %s (marker present, unreadable)"
              free-agent-bus-name root))
     (t
      (format "not attached: run `%s bus attach %s --root %s'"
              free-agent-bus-executable free-agent-bus-name root)))))

(defun free-agent-bus--audience-string (to &optional when-empty)
  "Render a post's TO field (a vector or list) as an audience string.
An empty TO on a post is `public', which is the CLI's own spelling for one.
WHEN-EMPTY is what an empty audience says for a caller describing something that
is not a post — a draft that has not been addressed says `none', because nothing
has been chosen, and calling that public would report a send nobody made."
  (let ((names (append to nil)))
    (if names (mapconcat #'identity names ",") (or when-empty "public"))))

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

(defun free-agent-bus--sender-in-line (line)
  "Return the sender name LINE carries, or nil.
Understands the same shapes `free-agent-bus--id-in-line' does: a raw JSONL
line (\"from\"), a log-tail head line (`[id@ts] from→audience'), and a CLI
view head line (`id from→audience')."
  (or (and (string-match "\"from\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" line)
           (match-string 1 line))
      (and (string-match "\\`\\[[0-9]+@[^]]*\\][ \t]+\\([^ \t]+\\)→" line)
           (match-string 1 line))
      (and (string-match "\\`[0-9]+[ \t]+\\([^ \t]+\\)→" line)
           (match-string 1 line))))

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

(defun free-agent-bus--sender-at-point ()
  "Return the sender name on the current line, or nil.
A line the deck rendered carries `free-agent-bus-sender';
`free-agent-bus--sender-in-line' reads the name back from the text otherwise."
  (free-agent-bus--field-at-point 'free-agent-bus-sender #'free-agent-bus--sender-in-line))

(defvar free-agent-bus--last-visited nil
  "The last post opened or replied to: a plist (:id :sender :root), or nil.
What a mark's reply-to-sender default falls back to when the mark is not
fired on a post itself — the same record RET, SPC y t and r keep.  The
:root slot ties the visit to the bus it was made on: a tracker from
another root is a different bus and invalidates the fallback.")

(defun free-agent-bus--visit-post (id sender)
  "Record ID and SENDER as `free-agent-bus--last-visited'.
Both are required: a reply target without a sender cannot name its
respondent, so such a visit is dropped rather than half-kept.  The
current `free-agent-bus-root' rides along; a root change invalidates
the record — a ~/bus visit never targets ~/dev/bus."
  (when (and id sender)
    (setq free-agent-bus--last-visited
          (list :id id :sender sender :root free-agent-bus-root))))

(defun free-agent-bus--sender-for-id (id)
  "Return the sender of post ID in the log, or nil when the log lacks it.
The prompt path of SPC y t knows an id without a buffer line; the log's
own `from' field is the authority, never a timestamp or a name match."
  (when (file-readable-p (free-agent-bus--log-file))
    (catch 'found
      (dolist (line (split-string (free-agent-bus-log--read-file) "\n" t))
        (when-let ((obj (ignore-errors (json-read-from-string line))))
          (when (equal (alist-get 'id obj) id)
            (throw 'found (alist-get 'from obj))))))))

;;; Failures stay visible

(defun free-agent-bus--report-failure (command status output)
  "Show a failed CLI invocation without discarding the runner's work."
  (let ((buf (get-buffer-create "*free-agent-bus-error*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "⛔ free-agent %s exited %s\n%s\n\n%s\n"
                        command status (free-agent-bus--context-line)
                        (if (string-empty-p output) "(no output)" output)))
        (special-mode)))
    (display-buffer buf)
    (message "⛔ free-agent %s failed (exit %s); see *free-agent-bus-error*"
             command status)))

;;; Rendering keeps the reader's place

(defun free-agent-bus--post-line-start (id)
  "Start of the line carrying post ID in the current buffer, or nil.
The property is what every rendered post carries, so this searches for the
property rather than for the text: a line re-rendered at another depth, or by
the CLI, still finds the same id."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (point-min))
          (found nil))
      (while (and (not found) (< pos (point-max)))
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

;;; Composition

(defvar-local free-agent-bus-post--origin nil
  "How the current draft was aimed: a plist (:id :sender :kind), or nil.
kind is `at-point' for a post at point, `visited' for the last-visited
fallback, `own-post' for a candidate naming this deck itself, `manual'
for an explicit C-c C-a / C-c C-t change; nil means no reply context —
the header says so rather than dressing an audience up as a reply.")

(defvar-local free-agent-bus-post--region-note nil
  "Note riding the draft's header line when a mark opened it without a
region: the line at point was quoted instead, and the header says so.")

(defvar-local free-agent-bus-post--manual-armed nil
  "Whether an explicit audience or thread applies to the next drafted mark.
The setters arm this one-shot choice.  A non-instant mark consumes it instead
of replacing the choice with an automatic reply target; later marks resolve
normally, so an old manual audience cannot become sticky.")

(defvar free-agent-bus-post--carried nil
  "Audience stashed when a mark opened the draft with no reply target:
the recipient list or public flag that was live, cleared so nothing
stale can fire, and offered — never silently applied — as the C-c C-a
prompt's initial input.")

(defun free-agent-bus--post-header-line ()
  "Header line for the composition buffer: the respondent, named or exposed.
Origin drives the variant; the recipient and thread variables are never
read on their own.  A resolved reply names sender, provenance and thread;
`own post' and no target are named directly.  The carried audience, when
there was one, waits in the C-c C-a prompt's initial input, never applied
silently.  A no-region note, when a mark opened the draft without a region,
rides along."
  (let ((note (if free-agent-bus-post--region-note
                  (concat " · " free-agent-bus-post--region-note)
                ""))
        (kind (plist-get free-agent-bus-post--origin :kind)))
    (cond
     (free-agent-bus-post-public
      (format "⟜ public — everyone · thread %s%s"
              (or free-agent-bus-post-thread "—") note))
     ((eq kind 'at-point)
      (format "⟜ reply to %s (post at point) · thread %s%s"
              (plist-get free-agent-bus-post--origin :sender)
              (or (plist-get free-agent-bus-post--origin :id) "—")
              note))
     ((eq kind 'visited)
      (format "⟜ reply to %s (last visited) · thread %s%s"
              (plist-get free-agent-bus-post--origin :sender)
              (plist-get free-agent-bus-post--origin :id)
              note))
     ((eq kind 'own-post)
      (format "⟜ own post — no reply target%s" note))
     ((and (eq kind 'manual) free-agent-bus-post-to)
      (format "⟜ to %s (set here) · thread %s%s"
              (mapconcat #'identity free-agent-bus-post-to ",")
              (or free-agent-bus-post-thread "—") note))
     (t
      (format "⟜ no reply target%s" note)))))

(defun free-agent-bus--post-mode-name ()
  "Mode-line name for the composition buffer, naming the audience."
  (format "FAB-Post[%s%s%s]"
          (cond (free-agent-bus-post-public "public")
                (free-agent-bus-post-to (mapconcat #'identity free-agent-bus-post-to ","))
                (t "no-recipients"))
          (if free-agent-bus-post-thread
              (format " thread=%s" free-agent-bus-post-thread)
            "")
          ""))

(defun free-agent-bus--post-mode-refresh (&optional buffer)
  "Update the composition BUFFER's mode line and header line after audience
or ancestry changes.  BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq mode-name (free-agent-bus--post-mode-name))
    (setq header-line-format (free-agent-bus--post-header-line))
    (force-mode-line-update)))

(defvar free-agent-bus-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'free-agent-bus-send)
    (define-key map (kbd "C-c C-a") #'free-agent-bus-set-recipients)
    (define-key map (kbd "C-c C-t") #'free-agent-bus-set-thread)
    map)
  "Keymap for `free-agent-bus-post-mode'.")

;; Ensure bindings survive reloads (defvar does not overwrite a live map).
(define-key free-agent-bus-post-mode-map (kbd "C-<return>") #'free-agent-bus-send)
(define-key free-agent-bus-post-mode-map (kbd "C-c C-a") #'free-agent-bus-set-recipients)
(define-key free-agent-bus-post-mode-map (kbd "C-c C-t") #'free-agent-bus-set-thread)

(defun free-agent-bus--post-note-manual ()
  "Arm the composition buffer's explicit targeting for the next mark.
Sets origin `manual', makes the one-shot choice, and refreshes the header —
invoked from any buffer, as the setters are global commands.  Creating the
composition buffer here makes SPC y a before the first composition work too."
  (with-current-buffer (get-buffer-create free-agent-bus-post-buffer-name)
    (setq-local free-agent-bus-post--origin (list :kind 'manual)
                free-agent-bus-post--manual-armed t)
    (free-agent-bus--post-mode-refresh)))

(defun free-agent-bus--take-manual-target ()
  "Consume and return the next mark's explicit target, or nil.
The audience and ancestry themselves stay in their existing variables.  The
returned provenance only tells the draft header that they were set manually."
  (when-let ((buffer (get-buffer free-agent-bus-post-buffer-name)))
    (with-current-buffer buffer
      (prog1 (when free-agent-bus-post--manual-armed (list :kind 'manual))
        (setq-local free-agent-bus-post--manual-armed nil)))))

;;;###autoload
(defun free-agent-bus-set-recipients (recipients)
  "Set the explicit audience for this post.
RECIPIENTS is a comma or space separated list of names.  The single word
`public' addresses the post to everyone (`to: []').  Empty input clears
the audience; a post needs an audience or public before it will send.
Sets the composition origin to `manual' — the header's reply variants
come from the mark's resolution alone, never from these variables."
  (interactive (list (read-string "Recipients (comma separated, or 'public'): "
                                  nil nil
                                  (cond (free-agent-bus-post-public "public")
                                        (free-agent-bus-post-to
                                         (mapconcat #'identity free-agent-bus-post-to ","))
                                        (free-agent-bus-post--carried
                                         free-agent-bus-post--carried)
                                        (t "")))))
  (let ((trimmed (string-trim recipients)))
    (cond
     ((string-empty-p trimmed)
      (setq free-agent-bus-post-public nil
            free-agent-bus-post-to nil
            free-agent-bus-post--carried nil))
     ((string= (downcase trimmed) "public")
      (setq free-agent-bus-post-public t
            free-agent-bus-post-to nil
            free-agent-bus-post--carried nil))
     (t
      (setq free-agent-bus-post-public nil
            free-agent-bus-post-to (split-string trimmed "[ ,]+" t)
            free-agent-bus-post--carried nil))))
  (free-agent-bus--post-note-manual)
  (when-let ((buffer (get-buffer free-agent-bus-post-buffer-name)))
    (free-agent-bus--post-mode-refresh buffer))
  (message "Audience: %s" (free-agent-bus--current-audience)))

(defun free-agent-bus-set-thread (id)
  "Set the post this composition answers.
ID is a post id; empty input clears the ancestry.  The post id is the task
id, so replying into a task's thread is how a CLAIM or DONE is filed.
Sets the composition origin to `manual', as `free-agent-bus-set-recipients'
does."
  (interactive (list (let ((input (read-string "Thread (post id, empty for a root post): "
                                              nil nil
                                              (if free-agent-bus-post-thread
                                                  (number-to-string free-agent-bus-post-thread)
                                                ""))))
                       (if (string-empty-p (string-trim input))
                           nil
                         (string-to-number (string-trim input))))))
  (setq free-agent-bus-post-thread id)
  (free-agent-bus--post-note-manual)
  (when-let ((buffer (get-buffer free-agent-bus-post-buffer-name)))
    (free-agent-bus--post-mode-refresh buffer))
  (message "Thread: %s" (or free-agent-bus-post-thread "root post")))

(defun free-agent-bus--current-audience ()
  "Audience of the next post as a string, for the mode line and messages.
Three outcomes from two branches: the public flag, then the same renderer the
log uses, with `none' as the empty case — an unaddressed draft is not a public
post, and the mode line has to say so before the send refuses it."
  (if free-agent-bus-post-public
      "public"
    (free-agent-bus--audience-string free-agent-bus-post-to "none")))

(defun free-agent-bus--receipt-line (output)
  "Turn the CLI's stamped post JSON into a one-line receipt."
  (let ((obj (ignore-errors (json-read-from-string (car (split-string output "\n"))))))
    (if obj
        (format "id=%s → %s"
                (alist-get 'id obj)
                (free-agent-bus--audience-string (alist-get 'to obj)))
      (string-trim output))))

;;;###autoload
(defun free-agent-bus-send (begin end)
  "Send the active region (or the whole post buffer) to the bus as an explicit act.
The post carries sender `free-agent-bus-name', the explicit audience from
`free-agent-bus-post-to' (or public), and the optional ancestry from
`free-agent-bus-post-thread'.  Sends through `free-agent bus say'; `bus post'
remains the JSON escape hatch.  On success only this composition buffer
clears; on failure the draft is kept and the error buffer is shown."
  (interactive "r")
  (let* ((use-region (use-region-p))
         (begin (if use-region begin (point-min)))
         (end (if use-region end (point-max))))
    (when (= begin end)
      (user-error "Nothing to send"))
    (unless (or use-region (derived-mode-p 'free-agent-bus-post-mode))
      (user-error "No active region; switch to free-agent-bus-post to send a whole buffer"))
    (let* ((body (string-trim (buffer-substring-no-properties begin end)))
           (args (append (list "--from" free-agent-bus-name)
                         (seq-mapcat (lambda (r) (list "--to" r)) free-agent-bus-post-to)
                         (when free-agent-bus-post-thread
                           (list "--thread" (number-to-string free-agent-bus-post-thread))))))
      (when (string-empty-p body)
        (user-error "Nothing to send"))
      (unless (or free-agent-bus-post-public free-agent-bus-post-to)
        (user-error "No reply target — C-c C-a sets one, or C-u on the mark sends instantly"))
      (let ((result (apply #'free-agent-bus--cli-stdin body "say" args)))
        (if (zerop (car result))
            (progn
              (message "%s [%s] posted · %s"
                       free-agent-bus-name (free-agent-bus--current-audience)
                       (free-agent-bus--receipt-line (cdr result)))
              (free-agent-bus-log-refresh t)
              (when (derived-mode-p 'free-agent-bus-post-mode)
                (erase-buffer)
                (free-agent-bus--post-mode-refresh)))
          (free-agent-bus--report-failure "bus say" (car result) (cdr result)))))))

(defun free-agent-bus--resolve-reply-target (&optional from to)
  "Resolve the reply target for a mark fired near FROM/TO: (:id :sender :kind).
Reads the rendered posts' own text properties only — a line in a card or
a note shaped like a bus post invents nothing.  Order: the post at the
region's begin, then at point — kind `at-point'; else the last-visited
post, when it belongs to this root — kind `visited'; else nil.  A
candidate whose sender is `free-agent-bus-name' is not a reply target —
kind `own-post', with no fallback to older or carried targets."
  (let ((positions (delete-dups (delq nil (list from to))))
        (reply nil))
    (while (and positions (not reply))
      (let* ((pos (pop positions))
             (id (get-text-property pos 'free-agent-bus-post-id))
             (sender (and id (get-text-property pos 'free-agent-bus-sender))))
        (when (and id sender)
          (setq reply (list :id id :sender sender :kind 'at-point)))))
    (when (and reply (equal (plist-get reply :sender) free-agent-bus-name))
      (plist-put reply :kind 'own-post))
    (or reply
        (let ((visited free-agent-bus--last-visited))
          (when (and visited
                     (equal (plist-get visited :root) free-agent-bus-root)
                     (plist-get visited :sender))
            (if (equal (plist-get visited :sender) free-agent-bus-name)
                (list :id (plist-get visited :id)
                      :sender (plist-get visited :sender)
                      :kind 'own-post)
              (list :id (plist-get visited :id)
                    :sender (plist-get visited :sender)
                    :kind 'visited)))))))

;;;###autoload
(defun free-agent-bus-mark (begin end &optional instant)
  "Send the region (or the line at point) as a mark: agent support requested
here.  The post body pins the place three ways — the buffer, the 1-based
line and the begin/end buffer positions (end exclusive), and the region
text itself.  The quoted region is the durable anchor; the positions are
exact only at send time and drift after later edits, so a seat acks
beneath the quote, never beneath the numbers.

Without a C-u prefix the mark stops: it opens the composition buffer
pre-filled with the body above and a header line naming the respondent.
An audience or thread explicitly set with SPC y a / SPC y t applies to this
next drafted mark once; it is neither replaced nor cleared by automatic
resolution.  Otherwise the respondent is resolved, not inherited: the post
at point — a rendered text property, never a line's shape — else the last
post visited (RET, SPC y t, r) on this bus root; else no target, whereupon
the live audience and ancestry are cleared, the old audience waits in
C-c C-a's prompt initial input, and the header says `no reply target'.
A candidate naming this deck itself is not a reply target either: `own post
— no reply target', the same clearing rule.  With no region, the line at
point is quoted and the header says so; nothing reaches the bus until
C-<return> sends the whole buffer through `free-agent-bus-send', which
refuses with no target at all.  With C-u, the mark fires instantly through
`free-agent bus say', exactly as before.  The source buffer is never erased
— a mark is a request about a card, not a composition act."
  (interactive
   (list (if (use-region-p) (region-beginning) (line-beginning-position))
         (if (use-region-p) (region-end) (line-end-position))
         current-prefix-arg))
  (let* ((use-region (use-region-p))
         (begin (if use-region begin (line-beginning-position)))
         (end (if use-region end (line-end-position)))
         (text (string-trim (buffer-substring-no-properties begin end))))
    (when (string-empty-p text)
      (user-error "Nothing to mark"))
    (let* ((line-no (line-number-at-pos begin))
           (end-line (line-number-at-pos end))
           (lines (if (= line-no end-line)
                      (format "line %d" line-no)
                    (format "lines %d-%d" line-no end-line)))
           (file (buffer-file-name))
           (body (concat (format "MARK %s · %s · chars %d..%d"
                                 (buffer-name) lines begin end)
                         (when file (format "\nfile: %s" file))
                         "\n\n" text))
           (target (unless instant
                     (or (free-agent-bus--take-manual-target)
                         (free-agent-bus--resolve-reply-target begin (point))))))
      (if instant
          (progn
            (unless (or free-agent-bus-post-public free-agent-bus-post-to)
              (user-error "No audience: set recipients first (C-c C-a) or 'public'"))
            (let* ((args (append (list "--from" free-agent-bus-name)
                                 (seq-mapcat (lambda (r) (list "--to" r)) free-agent-bus-post-to)
                                 (when free-agent-bus-post-thread
                                   (list "--thread" (number-to-string free-agent-bus-post-thread))))))
              (let ((result (apply #'free-agent-bus--cli-stdin body "say" args)))
                (if (zerop (car result))
                    (progn
                      (message "%s [%s] marked · %s"
                               free-agent-bus-name (free-agent-bus--current-audience)
                               (free-agent-bus--receipt-line (cdr result)))
                      (free-agent-bus-log-refresh t))
                  (free-agent-bus--report-failure "bus say" (car result) (cdr result))))))
        (cond
         ((and target (memq (plist-get target :kind) '(at-point visited)))
          (setq free-agent-bus-post-public nil
                free-agent-bus-post-to (list (plist-get target :sender))
                free-agent-bus-post-thread (plist-get target :id)
                free-agent-bus-post--carried nil))
         ((eq (plist-get target :kind) 'manual)
          (setq free-agent-bus-post--carried nil))
         (t
          (setq free-agent-bus-post--carried
                (cond (free-agent-bus-post-public "public")
                      (free-agent-bus-post-to
                       (mapconcat #'identity free-agent-bus-post-to ","))
                      (t nil))
                free-agent-bus-post-public nil
                free-agent-bus-post-to nil
                free-agent-bus-post-thread nil)))
        (let ((buf (get-buffer-create free-agent-bus-post-buffer-name)))
          (with-current-buffer buf
            (erase-buffer)
            (unless (derived-mode-p 'free-agent-bus-post-mode)
              (free-agent-bus-post-mode))
            (setq-local free-agent-bus-post--origin target
                        free-agent-bus-post--region-note (unless use-region
                                                           "no region — line at point"))
            (insert body "\n")
            (free-agent-bus--post-mode-refresh))
          (pop-to-buffer buf)
          (message "Mark drafted in %s — %s, C-<return> sends%s"
                   free-agent-bus-post-buffer-name
                   (if use-region "type beneath the quote" "no region — line at point quoted")
                   (if (or free-agent-bus-post-public free-agent-bus-post-to)
                       ""
                     " (no reply target — C-c C-a)")))))))

(define-derived-mode free-agent-bus-post-mode text-mode (free-agent-bus--post-mode-name)
  "Major mode for composing free-agent bus posts.
\\{free-agent-bus-post-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;;###autoload
(defun free-agent-bus-post ()
  "Open the composition buffer."
  (interactive)
  (pop-to-buffer free-agent-bus-post-buffer-name)
  (unless (derived-mode-p 'free-agent-bus-post-mode)
    (free-agent-bus-post-mode))
  (setq-local free-agent-bus-post--origin nil
              free-agent-bus-post--region-note nil)
  (free-agent-bus--post-mode-refresh))

;;; Board file

;;;###autoload
(defun free-agent-bus-board ()
  "Open the board file as a regular file."
  (interactive)
  (let ((path (expand-file-name free-agent-bus-board-file)))
    (unless (file-readable-p path)
      (user-error "Board file not found: %s (set free-agent-bus-board-file; SPC y r is the READY view)" path))
    (find-file path)))

;;; CLI-backed views: status, READY, thread

(defvar-local free-agent-bus--view-spec nil
  "Plist describing this CLI-backed view: :title :subcommand :args :base-depth.")

(defvar-local free-agent-bus-view-depth 1
  "Cascade depth of this view: 0 dot, 1 one-line excerpt, 2 verbatim.")

(defun free-agent-bus--view-head (line)
  "Return (ID . HEAD-END) for a CLI view line, or nil.
A view line's head is `id from→audience'.  A name may carry the READY view's
` (human)' label, so the audience is read as names rather than as one token;
HEAD-END is the position just after it, where an excerpt would begin."
  (when (string-match
         (concat "\\`\\([0-9]+\\)[ \t]+[^ \t]+\\(?:[ \t]+(human)\\)?→"
                 "\\(?:[^ ,\t]+\\(?:[ \t]+(human)\\)?"
                 "\\(?:,[^ ,\t]+\\(?:[ \t]+(human)\\)?\\)*\\)")
         line)
    (cons (string-to-number (match-string 1 line)) (match-end 0))))

(defun free-agent-bus--decorate-view-line (line posts)
  "Propertize one CLI view LINE against index POSTS.
An ancestry mark is added in both shapes a view renders: `↩ROOT' after a
head's audience, with a ` · ' before its excerpt — the shape the log tail uses —
and after a raw frame (depth 2), outside its braces, so the frame itself stays
exact.  A line carrying neither an id nor a sender is returned verbatim, so a
view never invents a post."
  (let ((id (free-agent-bus--id-in-line line))
        (from (free-agent-bus--sender-in-line line)))
    (if (or (null id) (null from))
        line
      (let* ((head (free-agent-bus--view-head line))
             (ancestry (free-agent-bus-log--ancestry-for id posts))
             (ancestry-start (if head (1+ (cdr head)) (1+ (length line))))
             (text (cond
                    ((string-empty-p ancestry) (copy-sequence line))
                    ((null head) (concat line " " ancestry))
                    (t (let* ((end (cdr head))
                              (rest (string-trim-left (substring line end))))
                         (concat (substring line 0 end) " " ancestry
                                 (if (string-empty-p rest) "" (concat " · " rest)))))))
             (mine (string= from free-agent-bus-name)))
        (add-text-properties 0 (length text)
                             (list 'free-agent-bus-post-id id
                                   'free-agent-bus-sender from
                                   'help-echo (format "RET: open the thread of post %s" id))
                             text)
        (when mine
          (add-text-properties 0 (length text)
                               (list 'face 'free-agent-bus-post-mine-face)
                               text))
        (unless (string-empty-p ancestry)
          (add-text-properties ancestry-start (+ ancestry-start (length ancestry))
                               (list 'face 'free-agent-bus-thread-face)
                               text))
        text))))

(defun free-agent-bus--decorate-view-output (output posts)
  "Propertize a CLI view's OUTPUT line by line, against index POSTS."
  (mapconcat (lambda (line) (free-agent-bus--decorate-view-line line posts))
             (split-string output "\n")
             "\n"))

(defun free-agent-bus--view-refresh ()
  "Re-run this view's CLI command and rerender the buffer.
A failing command is rendered into the buffer with its exit status; the
buffer is never left silently stale."
  (let* ((spec free-agent-bus--view-spec)
         (sub (plist-get spec :subcommand))
         (depth (or free-agent-bus-view-depth (plist-get spec :base-depth) 1))
         (result (apply #'free-agent-bus--cli sub
                        (append (plist-get spec :args)
                                (list "--depth" (number-to-string depth)))))
         (status (car result))
         (output (cdr result))
         (posts (free-agent-bus-log--read-index))
         (inhibit-read-only t))
    (free-agent-bus--preserving-position
      (erase-buffer)
      (insert (propertize (format "%s · depth %s\n%s\n\n"
                                  (plist-get spec :title) depth
                                  (free-agent-bus--context-line))
                          'face 'bold))
      (insert (if (string-empty-p output)
                  (propertize "(no rows)\n" 'face 'shadow)
                (concat (free-agent-bus--decorate-view-output output posts) "\n")))
      (when (/= status 0)
        (insert (propertize (format "\n⛔ free-agent bus %s exited %d\n" sub status) 'face 'error))))
    (set-buffer-modified-p nil)))

(defun free-agent-bus-cycle-depth ()
  "Cycle the cascade depth of the current view: 0 → 1 → 2 → 0."
  (interactive)
  (let* ((current (cond ((derived-mode-p 'free-agent-bus-log-mode) free-agent-bus-log-depth)
                        ((derived-mode-p 'free-agent-bus-inbox-mode) free-agent-bus-inbox-depth)
                        ((derived-mode-p 'free-agent-bus-view-mode) free-agent-bus-view-depth)
                        (t 1)))
         (next (cond ((= current 0) 1) ((= current 1) 2) (t 0))))
    (cond ((derived-mode-p 'free-agent-bus-log-mode)
           (setq free-agent-bus-log-depth next)
           (free-agent-bus-log-refresh t)
           (message "Depth %s" next))
          ((derived-mode-p 'free-agent-bus-inbox-mode)
           (setq free-agent-bus-inbox-depth next)
           (free-agent-bus-inbox-refresh)
           (message "Depth %s" next))
          ((derived-mode-p 'free-agent-bus-status-mode)
           (message "Status rows use depth 1"))
          ((derived-mode-p 'free-agent-bus-view-mode)
           (setq free-agent-bus-view-depth next)
           (free-agent-bus--view-refresh)
           (message "Depth %s" next))
          (t (message "Depth applies inside the log and view buffers")))))

(defvar free-agent-bus-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'free-agent-bus-thread-at-point)
    (define-key map (kbd "<tab>") #'free-agent-bus-cycle-depth)
    ;; TAB is the C-i event: a terminal frame sends it for the Tab key, where a
    ;; GUI frame sends <tab>.  Both names, or the key is dead in a terminal.
    (define-key map (kbd "TAB") #'free-agent-bus-cycle-depth)
    (define-key map (kbd "t") #'free-agent-bus-thread-at-point)
    map)
  "Keymap for CLI-backed bus views.")

(define-key free-agent-bus-view-mode-map (kbd "RET") #'free-agent-bus-thread-at-point)
(define-key free-agent-bus-view-mode-map (kbd "<tab>") #'free-agent-bus-cycle-depth)
(define-key free-agent-bus-view-mode-map (kbd "TAB") #'free-agent-bus-cycle-depth)

(define-derived-mode free-agent-bus-view-mode special-mode "FAB-View"
  "Major mode for CLI-backed bus views (status, READY, thread).
\\{free-agent-bus-view-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (free-agent-bus--view-refresh)))
  (setq-local truncate-lines nil))

(defun free-agent-bus--open-view (buffer-name title subcommand args depth)
  "Open BUFFER-NAME showing `bus SUBCOMMAND ARGS --depth DEPTH'.
TITLE names the view; the header carries root, identity, and freshness."
  (free-agent-bus--check-root)
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'free-agent-bus-view-mode)
        (free-agent-bus-view-mode))
      (setq-local free-agent-bus--view-spec
                  (list :title title :subcommand subcommand :args args :base-depth depth))
      (setq-local free-agent-bus-view-depth depth)
      (free-agent-bus--view-refresh))
    (pop-to-buffer buf)
    (free-agent-bus--start-auto-refresh)))

;;; Message-driven status

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

(define-derived-mode free-agent-bus-status-mode free-agent-bus-view-mode "FAB-Status"
  "Major mode for message-driven live bus status."
  (setq-local revert-buffer-function
              (lambda (&rest _) (free-agent-bus-status-refresh)))
  (local-set-key (kbd "q") #'kill-current-buffer)
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
     "^\\([🟢🟡⚪]+\\) \\([^ ]+\\) — \\([^;]+\\).*; cursor \\([^;]+\\); last post .*$"
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

(defun free-agent-bus-status--visible-row-p (row)
  "Whether ROW carries seat state or is a raw CLI row that must remain visible."
  (or (not (eq (plist-get row :kind) 'seat))
      (not (and (string= (plist-get row :freshness) "no heartbeat")
                (string= (plist-get row :cursor) "—")))))

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

;;;###autoload
(defun free-agent-bus-ready ()
  "Open the READY view: unclaimed open TASKs with no unresolved blocker.
Task ids are post ids; RET opens a task's thread and its verbatim body."
  (interactive)
  (free-agent-bus--open-view free-agent-bus-ready-buffer-name
                             "bus ready" "ready" '() 1))

;;;###autoload
(defun free-agent-bus-thread (id)
  "Open the thread view for post ID, following actual `thread' ancestry.
Depth 1 gives a quiet reading list; TAB reaches depth 2 for the verbatim log
frames when evidence rather than prose is wanted."
  (interactive (list (or (free-agent-bus--id-at-point)
                         (string-to-number (read-string "Thread id (post id): ")))))
  (unless (integerp id)
    (user-error "No post id at point and none given"))
  (unless (equal (plist-get free-agent-bus--last-visited :id) id)
    (free-agent-bus--visit-post id (free-agent-bus--sender-for-id id)))
  (free-agent-bus--open-view free-agent-bus-thread-buffer-name
                             (format "thread %s" id) "read"
                             (list "--thread" (number-to-string id)) 1))

;;;###autoload
(defun free-agent-bus-thread-at-point ()
  "Open the thread of the post at point, or offer to open any thread."
  (interactive)
  (let ((id (free-agent-bus--id-at-point))
        (sender (free-agent-bus--sender-at-point)))
    (if id
        (progn
          (free-agent-bus--visit-post id sender)
          (free-agent-bus-thread id))
      (call-interactively #'free-agent-bus-thread))))

;;; Inbox: unread mail for the human

(defvar-local free-agent-bus-inbox--watch nil
  "Async `bus ping-watch NAME' process owned by this inbox buffer.")

(defun free-agent-bus-inbox--cursor-path ()
  "The attached name's cursor file in the current bus root."
  (expand-file-name (concat ".cursor-" free-agent-bus-name)
                    (free-agent-bus--expand-root)))

(defun free-agent-bus-inbox--header ()
  "Header for the inbox buffer: identity, root, and the attached line."
  (format "bus inbox %s · depth %s\n%s\n%s\n\n"
          free-agent-bus-name
          free-agent-bus-inbox-depth
          (free-agent-bus--context-line)
          (free-agent-bus--attached-line)))

(defun free-agent-bus-inbox-refresh ()
  "Re-run the unread query and rerender this buffer.  Writes no cursor.
With an existing cursor the query is `bus inbox NAME'.  With no cursor the CLI
would start at the log tip and report no unread mail, so the deck instead uses
`bus read NAME --since 0': a fresh root reads from the first post without
creating state.  Marking read remains the separate, explicit
`free-agent-bus-inbox-mark-read' action, so a refresh — including one a ping
triggers — cannot consume mail the runner has not seen."
  (interactive)
  (free-agent-bus--check-root)
  (let* ((fresh (not (file-exists-p (free-agent-bus-inbox--cursor-path))))
         (result (if fresh
                     (free-agent-bus--cli "read" free-agent-bus-name
                                          "--since" "0" "--depth"
                                          (number-to-string free-agent-bus-inbox-depth))
                   (free-agent-bus--cli "inbox" free-agent-bus-name
                                        "--depth" (number-to-string free-agent-bus-inbox-depth))))
         (status (car result))
         (output (cdr result))
         (posts (free-agent-bus-log--read-index))
         (inhibit-read-only t))
    (free-agent-bus--preserving-position
      (erase-buffer)
      (insert (propertize (free-agent-bus-inbox--header) 'face 'bold))
      (insert (if (string-empty-p output)
                  (propertize "(no unread mail)\n" 'face 'shadow)
                (concat (free-agent-bus--decorate-view-output output posts) "\n")))
      (when (/= status 0)
        (insert (propertize (format "\n⛔ free-agent bus %s exited %d\n"
                                    (if fresh "read --since 0" "inbox") status)
                            'face 'error))))
    (set-buffer-modified-p nil)))

(defun free-agent-bus-inbox--ensure-cursor ()
  "Ensure a cursor exists for an explicit mark-read, without skipping history.
A missing cursor is initialized to post 0 through the CLI.  Ordinary inbox
refreshes never call this function."
  (if (file-exists-p (free-agent-bus-inbox--cursor-path))
      t
    (let ((result (free-agent-bus--cli '("cursor" "set") free-agent-bus-name "0")))
      (if (= (car result) 0)
          t
        (free-agent-bus--report-failure "bus cursor set 0" (car result) (cdr result))
        nil))))

;;;###autoload
(defun free-agent-bus-inbox-mark-read ()
  "Mark this name's unread mail read: `bus inbox NAME --mark-read'.
A missing cursor is first set to post 0, so this explicit action cannot skip
the history either.  The CLI then advances `.cursor-NAME' to the id after the
highest post printed and never past one, so anything arriving after this run
stays unread."
  (interactive)
  (free-agent-bus--check-root)
  (when (free-agent-bus-inbox--ensure-cursor)
    (let* ((result (free-agent-bus--cli "inbox" free-agent-bus-name "--mark-read"
                                        "--depth" (number-to-string free-agent-bus-inbox-depth)))
           (status (car result))
           (output (cdr result)))
      (if (/= status 0)
          (free-agent-bus--report-failure "bus inbox --mark-read" status output)
        (message "free-agent bus inbox: marked read (%s)"
                 (if (string-empty-p output)
                     "no unread mail"
                   (car (split-string output "\n"))))
        (free-agent-bus-inbox-refresh)))))

(defun free-agent-bus-inbox--watch-sentinel (proc _event)
  "Refresh this inbox when its ping-watch fires, then wait again."
  (when (memq (process-status proc) '(exit signal closed failed))
    (let ((buf (process-get proc 'free-agent-bus-buffer)))
      (when (and (buffer-live-p buf)
                 (eq proc (buffer-local-value 'free-agent-bus-inbox--watch buf)))
        (with-current-buffer buf
          (setq-local free-agent-bus-inbox--watch nil)
          (ignore-errors (free-agent-bus-inbox-refresh))
          (message "free-agent bus inbox: mail for %s" free-agent-bus-name)
          (ignore-errors (free-agent-bus-inbox-start-watch)))))))

(defun free-agent-bus-inbox-stop-watch ()
  "Stop this inbox buffer's ping-watch process."
  (when (process-live-p free-agent-bus-inbox--watch)
    (delete-process free-agent-bus-inbox--watch))
  (setq-local free-agent-bus-inbox--watch nil))

(defun free-agent-bus-inbox-start-watch ()
  "Wait for the next ping for this name, without polling.
`bus ping-watch NAME' blocks until the name's ping file changes and exits;
its sentinel refreshes the buffer and waits again.  No timer, no polling."
  (free-agent-bus-inbox-stop-watch)
  (let* ((buf (current-buffer))
         (proc (make-process
                :name (format "free-agent-ping-watch-%s" free-agent-bus-name)
                :buffer nil
                :command (list (free-agent-bus--executable) "bus" "ping-watch"
                               "--root" (free-agent-bus--expand-root)
                               free-agent-bus-name)
                :noquery t
                :sentinel #'free-agent-bus-inbox--watch-sentinel)))
    (process-put proc 'free-agent-bus-buffer buf)
    (setq-local free-agent-bus-inbox--watch proc)))

(defvar free-agent-bus-inbox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'free-agent-bus-thread-at-point)
    (define-key map (kbd "t") #'free-agent-bus-thread-at-point)
    (define-key map (kbd "r") #'free-agent-bus-reply-at-point)
    (define-key map (kbd "m") #'free-agent-bus-inbox-mark-read)
    (define-key map (kbd "g") #'free-agent-bus-inbox-refresh)
    (define-key map (kbd "<tab>") #'free-agent-bus-cycle-depth)
    (define-key map (kbd "TAB") #'free-agent-bus-cycle-depth)
    map)
  "Keymap for `free-agent-bus-inbox-mode'.")

;; The inbox map has no other top-level re-bind, and a `defvar' does not
;; overwrite a live map, so this one line is what keeps the key this slice adds
;; reachable after a reload rather than only after a restart.  The view and log
;; maps carry their own re-binds, and the status map lives off the view map's
;; parent, so those four surfaces come back on their own.
(define-key free-agent-bus-inbox-mode-map (kbd "TAB") #'free-agent-bus-cycle-depth)

(define-derived-mode free-agent-bus-inbox-mode special-mode "FAB-Inbox"
  "Major mode for the human's unread-mail buffer.
\\{free-agent-bus-inbox-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (free-agent-bus-inbox-refresh)))
  (add-hook 'kill-buffer-hook #'free-agent-bus-inbox-stop-watch nil t))

;;;###autoload
(defun free-agent-bus-reply-at-point ()
  "Reply into the thread of the post at point.
Sets the composition buffer's ancestry to that post id and its audience to
the post's sender, then opens the composition buffer.  Sending goes through
`bus say --thread ID', so the reply lands in the thread component rather
than beside every open offer."
  (interactive)
  (let ((id (free-agent-bus--id-at-point))
        (sender (free-agent-bus--sender-at-point)))
    (unless id
      (user-error "No post id at point — move to a post line first"))
    (free-agent-bus--visit-post id sender)
    (setq free-agent-bus-post-thread id)
    (when sender
      (setq free-agent-bus-post-public nil)
      (setq free-agent-bus-post-to (list sender)))
    (free-agent-bus-post)
    (when (and sender (get-buffer free-agent-bus-post-buffer-name))
      (with-current-buffer free-agent-bus-post-buffer-name
        (setq-local free-agent-bus-post--origin
                    (list :id id :sender sender :kind 'at-point)
                    free-agent-bus-post--region-note nil)
        (free-agent-bus--post-mode-refresh)))
    (message "Reply into thread %s%s" id
             (if sender (format " to %s" sender) " (no recipients set)"))))

;;;###autoload
(defun free-agent-bus-inbox ()
  "Open the human's inbox: unread posts addressed to `free-agent-bus-name'.
The header carries the explicit attached line.  Freshness waits on
`bus ping-watch NAME' rather than polling.  Opening and refreshing never move
`.cursor-NAME'; `m' marks read."
  (interactive)
  (free-agent-bus--check-root)
  (let ((buf (get-buffer-create free-agent-bus-inbox-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'free-agent-bus-inbox-mode)
        (free-agent-bus-inbox-mode))
      (free-agent-bus-inbox-refresh)
      (free-agent-bus-inbox-start-watch))
    (pop-to-buffer buf)))

;;; Auto refresh (visible views only; never a model, never a cursor)

(defvar free-agent-bus--auto-timer nil
  "Timer re-running visible CLI views.")

(defun free-agent-bus--auto-refresh-buffers ()
  "Refresh every visible CLI view; stop the timer when none is visible."
  (let ((visible nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and free-agent-bus--view-spec
                   (not (derived-mode-p 'free-agent-bus-status-mode))
                   (get-buffer-window buf))
          (setq visible t)
          (ignore-errors (free-agent-bus--view-refresh)))))
    (unless visible
      (free-agent-bus--stop-auto-refresh))))

(defun free-agent-bus--start-auto-refresh ()
  "Start the visible-view refresh timer, if it is not already running.
Refused, rather than armed, when the interval is not a positive number:
`run-with-timer' would take a timer that fires once and never repeats, so the
views would refresh once and then quietly stop being fresh — the same silent
failure the status timer guards against."
  (unless free-agent-bus--auto-timer
    (or (free-agent-bus--arm-timer 'free-agent-bus--auto-timer
                                   free-agent-bus-auto-refresh-interval
                                   free-agent-bus-auto-refresh-interval
                                   #'free-agent-bus--auto-refresh-buffers)
        (progn
          (message "free-agent-bus: auto-refresh interval %S cannot repeat; visible views will not refresh themselves"
                   free-agent-bus-auto-refresh-interval)
          nil))))

(defun free-agent-bus--stop-auto-refresh ()
  "Stop the visible-view refresh timer."
  (free-agent-bus--cancel-timer 'free-agent-bus--auto-timer))

;;; Log tail

(defvar free-agent-bus-log--timer nil
  "Fallback timer for periodic log refresh.
Holds the repeating poll only; file-notify events coalesce into
`free-agent-bus-log--debounce-timer', which fires once and is then cleared.  The
two are separate because their lifetimes differ, and one variable could not say
which of the two it held.")

(defvar free-agent-bus-log--debounce-timer nil
  "Pending coalescing timer for file-notify events on the log.
Armed by `free-agent-bus-log--refresh-soon' and cleared by the timer itself, so
it never holds a handle that has already fired.")

(defvar free-agent-bus-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar free-agent-bus-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'free-agent-bus-log-toggle-timestamps)
    (define-key map (kbd "f") #'free-agent-bus-log-set-filter)
    (define-key map (kbd "RET") #'free-agent-bus-thread-at-point)
    (define-key map (kbd "<tab>") #'free-agent-bus-cycle-depth)
    (define-key map (kbd "TAB") #'free-agent-bus-cycle-depth)
    map)
  "Keymap for `free-agent-bus-log-mode'.")

(define-key free-agent-bus-log-mode-map (kbd "RET") #'free-agent-bus-thread-at-point)
(define-key free-agent-bus-log-mode-map (kbd "<tab>") #'free-agent-bus-cycle-depth)
(define-key free-agent-bus-log-mode-map (kbd "TAB") #'free-agent-bus-cycle-depth)

(define-derived-mode free-agent-bus-log-mode text-mode "FreeAgentBus-Log"
  "Major mode for the bus log tail.
\\{free-agent-bus-log-mode-map}"
  (add-hook 'kill-buffer-hook #'free-agent-bus-log-stop-watch nil t))

(defun free-agent-bus-log--read-file (&optional start end)
  "Read the bus log file as UTF-8 text, optionally from START to END.
START and END are byte offsets, allowing an append to be read without
re-reading the log prefix."
  (with-temp-buffer
    (insert-file-contents-literally (free-agent-bus--log-file) nil start end)
    (set-buffer-file-coding-system 'utf-8)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (buffer-string)))

(defun free-agent-bus-log--shows-p (to)
  "Whether a post addressed to TO belongs in the current log buffer.
No filter shows everything; a filter shows public posts (`to: []') and any
post naming one of the filter names.  There is no broadcast token."
  (let ((names (append to nil))
        (filter free-agent-bus-log-filter))
    (or (null filter)
        (null names)
        (seq-intersection names filter))))

(defvar-local free-agent-bus-log--posts nil
  "Hash table of post id -> plist (:thread :label) for this buffer.
Filled from the log text as it is rendered, so ancestry resolves to a thread
root and that root's label without a CLI call per line; an appended post
inherits an index that already covers the prefix.  Two fields, because those
are the two things ancestry reads: the sender and the audience are in the
rendered line, and nothing reads them back out of this table.")

(defvar-local free-agent-bus-log--root nil
  "The expanded bus root this buffer's render state belongs to.
The index and the byte offsets are per-root, and `free-agent-bus-root' can be
repointed at another bus: without this, an id left over from the previous root
would resolve its ancestry against a post that is no longer being read, and an
offset would resume a different log mid-file.")

(defun free-agent-bus-log--repoint ()
  "Drop this buffer's render state when the deck has been repointed.
`free-agent-bus-log-refresh' calls this before it reads anything, so the tail
never renders a new root through the previous root's index or offsets."
  (let ((root (free-agent-bus--expand-root)))
    (unless (equal root free-agent-bus-log--root)
      (setq-local free-agent-bus-log--root root)
      (setq-local free-agent-bus-log--posts nil)
      (setq-local free-agent-bus-log--raw-offset nil)
      (setq-local free-agent-bus-log--newline-complete nil))))

(defun free-agent-bus-log--index-post (posts obj)
  "Record parsed post OBJ in index POSTS.  Returns OBJ."
  (when-let ((id (alist-get 'id obj)))
    (puthash id
             (list :thread (append (alist-get 'thread obj) nil)
                   :label (free-agent-bus-status--first-useful-line
                           (alist-get 'body obj)))
             posts))
  obj)

(defun free-agent-bus-log--index (obj)
  "Record parsed post OBJ in this buffer's index.  Returns OBJ.
The log buffer builds its index from the text it is rendering, so the index
covers the log prefix and every append, and no line needs its own file read.
The table is rebuilt on every full render and dropped on a repoint, so an id in
it always belongs to the log this buffer is showing."
  (unless free-agent-bus-log--posts
    (setq-local free-agent-bus-log--posts (make-hash-table :test 'eq)))
  (free-agent-bus-log--index-post free-agent-bus-log--posts obj))

(defun free-agent-bus-log--read-index ()
  "Return a fresh post index, read from the whole log file.
A view buffer has no tail to build an index from, so it reads the log once per
refresh — the way the status model already reads it for assignment labels.  A
missing or unreadable log yields an empty index; the view's own CLI call is what
reports that root as broken."
  (let ((posts (make-hash-table :test 'eq)))
    (when (file-readable-p (free-agent-bus--log-file))
      (dolist (line (split-string (free-agent-bus-log--read-file) "\n" t))
        (when-let ((obj (ignore-errors (json-read-from-string line))))
          (free-agent-bus-log--index-post posts obj))))
    posts))

(defun free-agent-bus-log--thread-root (id posts)
  "Return the root post id of ID's thread in index POSTS, or ID itself.
Only the log's own `thread' field is followed — never a timestamp, never a
sender name — and a cycle in that field stops the walk where it repeats."
  (let ((current id)
        (seen (list id))
        (next nil))
    (while (and (setq next (car (plist-get (gethash current posts) :thread)))
                (not (memq next seen)))
      (setq current next)
      (push next seen))
    current))

(defun free-agent-bus-log--ancestry-for (id posts)
  "Ancestry text for post ID in index POSTS: `↩ROOT', or the empty string.
ROOT is the thread root's id.  RET opens that conversation, so repeating the
root's own first line on every reply only obscures the post being read.  A post
with no ancestry, and a post POSTS does not carry, render no ancestry at all."
  (if-let ((thread (plist-get (gethash id posts) :thread)))
      (let ((root (free-agent-bus-log--thread-root id posts)))
        (concat "↩" (number-to-string root)))
    ""))

(defun free-agent-bus-log--format-line (line)
  "Format one raw log LINE at the buffer's cascade depth, carrying properties.
0 = dot, 1 = one-line excerpt, 2 = verbatim body.  Unparseable lines are
returned as-is so the runner sees exactly what is in the file, and a post
whose audience does not match is dropped.

A rendered post carries `free-agent-bus-post-id' and `free-agent-bus-sender',
which `free-agent-bus--id-at-point' and `free-agent-bus--sender-at-point' read
back for RET and for a reply.  A post the attached human wrote is marked by
`free-agent-bus-post-mine-face' on its summary (the head line, and at depth 1
the excerpt beside it) rather than by a property of its own, and its ancestry is
faced with `free-agent-bus-thread-face'."
  (let ((obj (ignore-errors (json-read-from-string line))))
    (cond
     ((null obj) line)
     (t
      (if (not (free-agent-bus-log--shows-p (alist-get 'to obj)))
          nil
        (let* ((id (alist-get 'id obj))
               (ts (alist-get 'ts obj))
               (from (alist-get 'from obj))
               (audience (free-agent-bus--audience-string (alist-get 'to obj)))
               (ancestry (free-agent-bus-log--ancestry-for
                          id free-agent-bus-log--posts))
               (body (or (alist-get 'body obj) ""))
               (mine (and (string= from free-agent-bus-name) t))
               (stamped (if free-agent-bus-log-show-timestamps
                            (format "[%s@%s]" id ts)
                          (format "[%s]" id)))
               (depth free-agent-bus-log-depth)
               (prefix (concat stamped " " from "→" audience))
               (head (concat prefix
                             (if (string-empty-p ancestry) "" (concat " " ancestry))))
               (ancestry-start (1+ (length prefix)))
               (text (pcase depth
                       (0 head)
                       (1 (concat head
                                  (if (string-empty-p ancestry) " " " · ")
                                  (truncate-string-to-width
                                   (car (split-string body "\n"))
                                   free-agent-bus-log-excerpt-width nil nil "…")))
                       (_ (concat head "\n"
                                  (mapconcat (lambda (l) (concat "│ " l))
                                             (split-string body "\n") "\n"))))))
          ;; Two properties, both read back: --id-at-point and --sender-at-point
          ;; are what RET, thread-at-point and reply-at-point ask.  The human's own
          ;; posts are marked by the face, so no property repeats that, and the
          ;; thread root is in the label text rather than in a property nothing
          ;; reads.
          (add-text-properties
           0 (length text)
           (list 'free-agent-bus-post-id id
                 'free-agent-bus-sender from
                 'help-echo (format "RET: open the thread of post %s" id))
           text)
          (when mine
            (add-text-properties 0 (if (= depth 1) (length text) (length head))
                                 (list 'face 'free-agent-bus-post-mine-face)
                                 text))
          (unless (string-empty-p ancestry)
            (add-text-properties ancestry-start (+ ancestry-start (length ancestry))
                                 (list 'face 'free-agent-bus-thread-face)
                                 text))
          text))))))

(defvar-local free-agent-bus-log--raw-offset nil
  "Byte offset of the log snapshot used for the current render.")

(defvar-local free-agent-bus-log--newline-complete nil
  "Whether the rendered log snapshot ends with a complete newline.")

(defun free-agent-bus-log--format-lines (raw)
  "Format RAW log text using the current buffer's display settings.
Every line is indexed before any line is formatted, so a post whose ancestry
names an id later in the same text still finds it, and a post the filter hides
still labels the posts the filter shows."
  (let ((lines (split-string raw "\n")))
    (dolist (line lines)
      (when-let ((obj (ignore-errors (json-read-from-string line))))
        (free-agent-bus-log--index obj)))
    (mapconcat #'identity
               (delq nil (mapcar #'free-agent-bus-log--format-line lines))
               "\n")))

(defun free-agent-bus-log--replace-contents (contents)
  "Replace the log buffer with CONTENTS, keeping the reader's place.
This is the forced path — a depth change, a truncation, a partial-line append —
so it is the one where every post changes length; the record resolves by post
rather than by offset for exactly that reason."
  (let ((state (free-agent-bus--remember-position)))
    (erase-buffer)
    (insert contents)
    (free-agent-bus--restore-position state)))

(defun free-agent-bus-log--append-contents (contents)
  "Append CONTENTS to the log buffer, keeping the reader's place.
Appending shifts nothing, so the record lands on the same lines it recorded; the
same mechanism serves both, which is the point: one idiom for the reader's
place, not the tail's own pair beside the shared macro."
  (let ((state (free-agent-bus--remember-position)))
    (save-excursion
      (goto-char (point-max))
      (insert contents))
    (free-agent-bus--restore-position state)))

(defun free-agent-bus-log-refresh (&optional force)
  "Refresh the log buffer from the file, preserving scroll position.
A newline-complete append is formatted and inserted without reading or
rendering the log prefix.  FORCE, truncation, and partial-line appends use
a full rebuild."
  (when-let ((buf (get-buffer free-agent-bus-log-buffer-name)))
    (with-current-buffer buf
      (free-agent-bus-log--repoint)
      (let* ((inhibit-read-only t)
             (size (file-attribute-size
                    (file-attributes (free-agent-bus--log-file))))
             (offset free-agent-bus-log--raw-offset)
             (incremental
              (and (not force)
                   offset
                   free-agent-bus-log--newline-complete
                   (> size offset))))
        (if incremental
            (let ((raw (free-agent-bus-log--read-file offset size)))
              (if (string-suffix-p "\n" raw)
                  (progn
                    (free-agent-bus-log--append-contents
                     (free-agent-bus-log--format-lines raw))
                    (setq free-agent-bus-log--raw-offset size))
                (setq incremental nil)))
          (when (and (not force) offset (= size offset))
            (setq incremental 'unchanged)))
        (unless incremental
          ;; A full render re-reads the whole log, so the index is rebuilt rather
          ;; than extended: a log replaced under this buffer (moved to the
          ;; archive, restarted from id 0) leaves no id behind.
          (setq-local free-agent-bus-log--posts nil)
          (let* ((raw (free-agent-bus-log--read-file 0 size))
                 (contents (free-agent-bus-log--format-lines raw)))
            (when (or force (not (equal-including-properties contents (buffer-string))))
              (free-agent-bus-log--replace-contents contents))
            (setq free-agent-bus-log--raw-offset size
                  free-agent-bus-log--newline-complete
                  (or (zerop size) (string-suffix-p "\n" raw)))))))))

(defun free-agent-bus-log--refresh-soon ()
  "Coalesce file-notification events into one log refresh.
Its own variable, not `free-agent-bus-log--timer': that one holds the polling
fallback, which repeats, while this one fires once — two lifetimes, two names."
  (free-agent-bus--debounce 'free-agent-bus-log--debounce-timer 0.05
                            #'free-agent-bus-log-refresh))

(defun free-agent-bus-log-toggle-timestamps ()
  "Toggle ids and timestamps in the log tail and re-render."
  (interactive)
  (setq free-agent-bus-log-show-timestamps (not free-agent-bus-log-show-timestamps))
  (message "free-agent-bus log timestamps %s"
           (if free-agent-bus-log-show-timestamps "on" "off"))
  (free-agent-bus-log-refresh t))

;;;###autoload
(defun free-agent-bus-log-set-filter (filter)
  "Set this log buffer's audience filter.
FILTER is a comma or space separated list of names, or empty to show every
post.  Public posts (`to: []') are always shown.  The retired `\"all\"' token
has no special meaning; name it explicitly to see legacy broadcast lines."
  (interactive
   (list (let ((choice (completing-read
                        (format-prompt "Audience filter (empty = all posts)"
                                       (if free-agent-bus-log-filter
                                           (mapconcat #'identity free-agent-bus-log-filter ",")
                                         ""))
                        (seq-uniq (append free-agent-bus-log-filter-choices
                                          (list free-agent-bus-name)))
                        nil nil nil nil
                        (if free-agent-bus-log-filter
                            (mapconcat #'identity free-agent-bus-log-filter ",")
                          ""))))
           (split-string (string-trim choice) "[ ,]+" t))))
  (setq-local free-agent-bus-log-filter
              (if (stringp filter) (list filter) filter))
  (message "Audience filter: %s"
           (if free-agent-bus-log-filter
               (mapconcat #'identity free-agent-bus-log-filter ",")
             "all posts"))
  (free-agent-bus-log-refresh t))

(defun free-agent-bus-log-start-watch ()
  "Start watching the log file.
Prefer file-notify events; fall back to a polling timer if unavailable.  The
fallback timer is refused, rather than armed, when the interval is not a
positive number: it would fire once and never again, so the tail would stop
refreshing itself without saying so."
  (free-agent-bus-log-stop-watch)
  (setq free-agent-bus-log--file-watch
        (free-agent-bus--watch-log-file
         #'free-agent-bus-log--refresh-soon
         (lambda ()
           (or (free-agent-bus--arm-timer 'free-agent-bus-log--timer 0
                                          free-agent-bus-log-refresh-interval
                                          #'free-agent-bus-log-refresh)
               (message "free-agent-bus: file-notify is unavailable and the log-refresh interval %S cannot repeat; the tail will refresh only when you refresh it"
                        free-agent-bus-log-refresh-interval))))))

(defun free-agent-bus-log-stop-watch ()
  "Stop watching the log file, and drop any refresh it still has pending."
  (free-agent-bus--cancel-timer 'free-agent-bus-log--timer)
  (free-agent-bus--cancel-timer 'free-agent-bus-log--debounce-timer)
  (when free-agent-bus-log--file-watch
    (file-notify-rm-watch free-agent-bus-log--file-watch)
    (setq free-agent-bus-log--file-watch nil)))

(defun free-agent-bus--log-buffer-setup ()
  "Prepare the log buffer: mode, read-only, watch, render."
  (free-agent-bus-log-mode)
  (setq buffer-read-only t)
  (free-agent-bus-log-start-watch)
  (free-agent-bus-log-refresh)
  (goto-char (point-max)))

;;;###autoload
(defun free-agent-bus-log ()
  "Open the log tail buffer and start tailing it."
  (interactive)
  (free-agent-bus--check-root)
  (let ((buf (get-buffer-create free-agent-bus-log-buffer-name)))
    (with-current-buffer buf
      (free-agent-bus--log-buffer-setup))
    (pop-to-buffer buf)))

;;; Deck

;;;###autoload
(defun free-agent-bus-deck ()
  "Open the deck: live status on top, log in the middle, composition below.
One frame, one rail: notice live work, read the evidence, post the next act."
  (interactive)
  (free-agent-bus--check-root)
  (delete-other-windows)
  (let* ((status-buf (get-buffer-create free-agent-bus-status-buffer-name))
         (log-buf (get-buffer-create free-agent-bus-log-buffer-name))
         (post-buf (get-buffer-create free-agent-bus-post-buffer-name))
         (main (selected-window)))
    (with-current-buffer status-buf
      (free-agent-bus-status--prepare))
    (set-window-buffer main status-buf)
    (set-window-point main (with-current-buffer status-buf (point-min)))
    (let ((log-win (split-window main (floor (* 0.30 (window-total-height main))))))
      (set-window-buffer log-win log-buf)
      (with-current-buffer log-buf
        (free-agent-bus--log-buffer-setup))
      (set-window-point log-win (with-current-buffer log-buf (point-max)))
      (let ((post-win (split-window log-win (floor (* 0.55 (window-total-height log-win))))))
        (set-window-buffer post-win post-buf)
        (with-current-buffer post-buf
          (unless (derived-mode-p 'free-agent-bus-post-mode)
            (free-agent-bus-post-mode))
          (free-agent-bus--post-mode-refresh))
        (select-window main)))))

;;;###autoload
(defun free-agent-bus-deck-quit ()
  "Close the deck buffers, stop the log watcher, and stop view refresh."
  (interactive)
  (free-agent-bus-log-stop-watch)
  (free-agent-bus--stop-auto-refresh)
  (when-let ((b (get-buffer free-agent-bus-inbox-buffer-name)))
    (with-current-buffer b (free-agent-bus-inbox-stop-watch)))
  (dolist (buf (list free-agent-bus-post-buffer-name
                     free-agent-bus-log-buffer-name
                     free-agent-bus-status-buffer-name
                     free-agent-bus-ready-buffer-name
                     free-agent-bus-inbox-buffer-name
                     free-agent-bus-thread-buffer-name))
    (when-let ((b (get-buffer buf)))
      (kill-buffer b))))

;; evil's state maps shadow the deck's own keys: m is evil-set-marker, r is
;; evil-replace, t is evil-find-char-to, RET is evil-ret, q is evil-record-macro
;; (Doom's override layer takes C-<return> in normal and visual state), so a
;; plain buffer-local major-mode map never sees them.  An evil-define-key on the
;; mode keymap installs an evil auxiliary keymap for that mode, and the
;; auxiliary map is consulted above the state maps and above the override
;; layer, so the mode's own keys win inside its buffer while evil, its state
;; maps and the leader stay untouched.  evil-define-key* is the immediate form
;; of evil-define-key, used under `with-eval-after-load' so evil stays out of
;; the deck's compile- and load-time dependencies; the aux keymap is created for
;; a symbol keymap whose value is set, which is the case here.
;;
;; Every documented key in this file appears in this function, so the tables in
;; the header comment and in buff/emacs.md describe what the deck actually
;; does.  `g' is deliberately absent: it lives in an evil intercept layer that
;; only an aux keymap could shadow, and it is worth more as evil's goto prefix
;; than as a refresh key — refreshing is automatic (log watch, status timer).
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))

(defun free-agent-bus--evil-bind-keys ()
  "Give the deck's modes their own keys back under evil.
Each binding mirrors the mode's own map; the aux keymaps are what make those
keys reachable while evil's state maps are active."
  (evil-define-key* 'normal free-agent-bus-post-mode-map
    (kbd "C-<return>") #'free-agent-bus-send)
  (evil-define-key* 'visual free-agent-bus-post-mode-map
    (kbd "C-<return>") #'free-agent-bus-send)
  (evil-define-key* 'normal free-agent-bus-log-mode-map
    (kbd "t") #'free-agent-bus-log-toggle-timestamps
    (kbd "f") #'free-agent-bus-log-set-filter
    (kbd "RET") #'free-agent-bus-thread-at-point
    (kbd "q") #'quit-window)
  (evil-define-key* 'normal free-agent-bus-view-mode-map
    (kbd "t") #'free-agent-bus-thread-at-point
    (kbd "RET") #'free-agent-bus-thread-at-point)
  (evil-define-key* 'normal free-agent-bus-inbox-mode-map
    (kbd "m") #'free-agent-bus-inbox-mark-read
    (kbd "r") #'free-agent-bus-reply-at-point
    (kbd "t") #'free-agent-bus-thread-at-point
    (kbd "RET") #'free-agent-bus-thread-at-point)
  (evil-define-key* 'normal free-agent-bus-status-mode-map
    (kbd "q") #'kill-current-buffer))

(with-eval-after-load 'evil (free-agent-bus--evil-bind-keys))

;; Leader bindings, self-contained: the deck binds its own keys whenever
;; `doom-leader-map' exists, so no Doom config edit is needed for a hot-load
;; or a restart.  A Doom `map!' block only adds which-key descriptions.
(when (boundp 'doom-leader-map)
  (dolist (pair '(("ya" . free-agent-bus-set-recipients)
                  ("ym" . free-agent-bus-mark)
                  ("yf" . free-agent-bus-log-set-filter)
                  ("ys" . free-agent-bus-status)
                  ("yr" . free-agent-bus-ready)
                  ("yi" . free-agent-bus-inbox)
                  ("yt" . free-agent-bus-thread-at-point)))
    (define-key doom-leader-map (kbd (car pair)) (cdr pair))))

(provide 'free-agent-bus-deck)
;;; free-agent-bus-deck.el ends here
