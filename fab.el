;;; fab.el --- fab: bus Org conversations, plain-file capable -*- lexical-binding: t; -*-

;; The first end-to-end slice of ~/coffee/loom/card-composer.md.  The bus
;; owns rendering (`free-agent bus org'); fab opens a thread in any Org
;; buffer — temporary or an existing plain file — carries root and thread
;; identity as ordinary text (#+fab-root / #+fab-thread), appends new
;; responses at a `** live [append]' marker when opted in, and converts a
;; sent `** NAME [send]' draft into an anchored post in place.  No
;; dedicated major mode beyond org-mode; buffer names are never read.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup fab nil
  "Bus Org conversations."
  :group 'applications)

(defcustom fab-executable "free-agent"
  "Executable used to render and post."
  :type 'string
  :group 'fab)

(defcustom fab-root "~/net"
  "Default bus root for `fab-open' buffers that carry no #+fab-root keyword."
  :type 'string
  :group 'fab)

(defcustom fab-name "runner"
  "Name the [send] draft posts as, and the owner of that draft."
  :type 'string
  :group 'fab)

;;; Identity ⟜ ordinary text, never buffer names

(defun fab--keyword (key)
  "The value of #+KEY in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]+\\(.+?\\)[ \t]*$" key) nil t)
      (string-trim (match-string-no-properties 1)))))

(defun fab-buffer-root ()
  "The bus root this buffer renders from: #+fab-root, or `fab-root' as the
explicit default.  Identity is text: a plain file keeps it across
kills and reopens."
  (let ((value (or (fab--keyword "fab-root") fab-root)))
    (expand-file-name value)))

(defun fab-buffer-thread ()
  "The thread id this buffer renders and posts to: #+fab-thread.
Every other function resolves through this, so a plain file is a
complete fab view with no buffer state."
  (let ((value (fab--keyword "fab-thread")))
    (unless value
      (user-error "No #+fab-thread keyword in this buffer — M-x fab-open or add the keyword"))
    (string-to-number value)))

;;; CLI

(defun fab--cli-stdin (input &rest args)
  "Run `fab-executable' bus ARGS with INPUT on stdin.  Return (STATUS . OUTPUT);
a failing command stays a value so the caller can name it."
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert input)
      (let ((reply-from (point-max)))
        (condition-case err
            (let ((status (apply #'call-process-region
                                 (point-min) reply-from
                                 fab-executable nil t nil
                                 "bus" args)))
              (cons status (string-trim-right
                            (buffer-substring reply-from (point-max)))))
          (file-error
           (cons 127 (format "cannot run %s: %s"
                             fab-executable (error-message-string err)))))))))

(defun fab--render (root thread)
  "Render ROOT/THREAD through `bus org'; signal a user-error carrying the
renderer's 🔴 line on failure."
  (let ((result (apply #'fab--cli-stdin
                       ""
                       (list "org" "-r" root "--thread" (number-to-string thread)))))
    (unless (zerop (car result))
      (user-error "%s" (cdr result)))
    (cdr result)))

;;; The marked insertion point

(defconst fab--live-marker "** live [append]"
  "The ordinary Org heading new responses append at.
Ordinary text on purpose: a plain file keeps the marker across
kills and reopens, and opting in fails loudly where it is absent.")

(defun fab--live-marker-position ()
  "Buffer position of the start of the `fab--live-marker' line, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^%s$" (regexp-quote fab--live-marker)) nil t)
      (line-beginning-position))))

;;; Post sections

(defconst fab--post-heading
  "^\\*\\* [^ ]+ \\[#\\([0-9]+\\)\\]"
  "A rendered post heading: `** from [#id] …'.  The id is the one stable
token both the renderer and this buffer agree on.")

(defun fab--post-ids-in-buffer ()
  "The post ids this buffer already carries, as a list of numbers."
  (save-excursion
    (goto-char (point-min))
    (let (ids)
      (while (re-search-forward fab--post-heading nil t)
        (push (string-to-number (match-string-no-properties 1)) ids))
      (nreverse ids))))

(defun fab--render-post-sections (rendered)
  "Split RENDERED into (ID . SECTION-TEXT) posts; the thread heading is not
a post and stays with the buffer."
  (let ((sections nil)
        (current nil))
    (dolist (line (split-string rendered "\n"))
      (if (string-match fab--post-heading line)
          (progn
            (when current (push (nreverse current) sections))
            (setq current (list line)))
        (when current (push line current))))
    (when current (push (nreverse current) sections))
    (mapcar (lambda (lines)
              (string-match fab--post-heading (car lines))
              (cons (string-to-number (match-string 1 (car lines)))
                    (string-join lines "\n")))
            (nreverse sections))))

;;; Open

(defun fab--id-at-point ()
  "The [#N] id bracketing point, or nil."
  (let* ((beg (max (point-min) (- (point) 16)))
         (end (min (point-max) (+ (point) 16)))
         (window (buffer-substring-no-properties beg end)))
    (when (string-match "\\[#\\([0-9]+\\)\\]" window)
      (string-to-number (match-string 1 window)))))

;;;###autoload
(defun fab-open (id &optional root)
  "Render thread ID from ROOT (default `fab-root') into a temporary Org
buffer, with #+fab-root/#+fab-thread keywords, a `** live [append]'
marker, and a `** fab-name [send]' draft.  ID defaults to the [#N]
bracketing point.  The buffer is plain org-mode; identity is text."
  (interactive
   (list (or (fab--id-at-point)
             (string-to-number
              (read-string "Thread id: " nil nil "" t)))))
  (unless (and (integerp id) (>= id 0))
    (user-error "No [#N] at point and no thread id given"))
  (let* ((root* (expand-file-name (or root fab-root)))
         (rendered (fab--render root* id)))
    (with-current-buffer (get-buffer-create (format "*fab %d*" id))
      (erase-buffer)
      (insert (format "#+fab-root: %s\n#+fab-thread: %d\n\n" root* id))
      (insert rendered)
      (unless (string-suffix-p "\n" (buffer-string))
        (insert "\n"))
      (insert "\n" fab--live-marker "\n\n** " fab-name " [send]\n")
      (org-mode)
      (goto-char (point-max)))
    (when (called-interactively-p 'any)
      (pop-to-buffer (format "*fab %d*" id)))))

;;; Send ⟜ receipted, in place, no re-render

(defun fab-send ()
  "Post the body of this buffer's `** fab-name [send]' section to the
enclosing thread, then convert the draft into an anchored post in
place: the heading becomes `** fab-name [#id]' from the receipt and
nothing else moves — no re-render, no refresh of any other text."
  (interactive)
  (let* ((root (fab-buffer-root))
         (thread (fab-buffer-thread)))
    (goto-char (point-min))
    (unless (re-search-forward
             (format "^\\*\\* %s \\[send\\]$" (regexp-quote fab-name))
             nil t)
      (user-error "No ** %s [send] section in this buffer" fab-name))
    ;; The body is every line after the send heading up to the next
    ;; heading-like line — except the first body line, which is always
    ;; included, so a draft quoting a line that begins with `*' is sent
    ;; verbatim instead of reading as empty.  A one-line star draft
    ;; followed by another star line ends at that second line; that is
    ;; the accepted limit of structure-versus-verbatim.
    (let* ((first-line-start (line-end-position))
           (body-end
            (save-excursion
              (goto-char first-line-start)
              (forward-line 1)          ; first body line: always included
              (forward-line 1)          ; checking starts at the second
              (while (and (not (eobp))
                          (not (looking-at-p "\\*+[ \t]")))
                (forward-line 1))
              (point)))
           (body (string-trim
                  (buffer-substring-no-properties first-line-start body-end))))
      (when (string-empty-p body)
        (user-error "Nothing to send — type beneath the ** %s [send] heading"
                    fab-name))
      ;; Unescape the render's Org protection (`,*' → `*', `,,' → `,')
      ;; so a rendered post copied into the draft posts the exact stored
      ;; body; anything not escaped passes through unchanged.
      (let* ((raw (string-join
                   (mapcar (lambda (l)
                             (cond
                              ((string-match "^,," l) (substring l 1))
                              ((string-match "^,\\*" l) (substring l 1))
                              (t l)))
                           (split-string body "\n"))
                   "\n"))
             (result (apply #'fab--cli-stdin
                            raw
                            (list "post" "-r" root
                                  "--from" fab-name
                                  "--thread" (number-to-string thread)))))
        (unless (zerop (car result))
          (user-error "free-agent bus post exited %s: %s"
                      (car result) (cdr result)))
        (let* ((json (ignore-errors (json-parse-string (car (split-string (cdr result) "\n")))))
               (id (and json (gethash "id" json))))
          (unless (and (numberp id) (> id 0))
            (user-error "No id in post receipt: %s" (cdr result)))
          (beginning-of-line)
          (kill-line)
          (insert (format "** %s [#%d]" fab-name id))
          (message "fab: posted [#%d] to thread %d" id thread))))))

;;; Live arrival ⟜ opt-in per view, append-only at the marker

(defvar-local fab--live-watch nil
  "File-notify descriptor on the bus log, or nil.")

(defvar-local fab--live-debounce nil
  "Pending coalescing timer for log events.")

;;;###autoload
(define-minor-mode fab-live-mode
  "Opt this buffer into live arrival: new responses to its thread append
at the `** live [append]' marker, in place, without rewriting other
text, clobbering edits, or saving a file-backed buffer.  Append-only:
the log is the record; this view never writes it."
  :lighter " FAB-Live"
  (if fab-live-mode
      (let ((root (fab-buffer-root))
            (buffer (current-buffer)))
        (fab-buffer-thread)
        (unless (fab--live-marker-position)
          (setq fab-live-mode nil)
          (user-error "No %s heading in this buffer" fab--live-marker))
        (unless (file-readable-p (expand-file-name "log.jsonl" root))
          (setq fab-live-mode nil)
          (user-error "fab: no readable log under %s" root))
        (setq fab--live-watch
              (file-notify-add-watch (expand-file-name "log.jsonl" root)
                                     '(change)
                                     (lambda (_event)
                                       (when (buffer-live-p buffer)
                                         (with-current-buffer buffer
                                           (fab--live-soon))))))
        (add-hook 'kill-buffer-hook #'fab-live--stop nil t))
    (fab-live--stop)))

(defun fab-live--stop ()
  "Drop the watch and any pending debounce."
  (when fab--live-watch
    (file-notify-rm-watch fab--live-watch)
    (setq fab--live-watch nil))
  (when fab--live-debounce
    (cancel-timer fab--live-debounce)
    (setq fab--live-debounce nil)))

(defun fab--live-soon ()
  "Coalesce log events into one arrival pass.  The timer fires in whatever
buffer happens to be current, so the originating buffer is captured here
and the refresh runs pinned to it — an unpinned callback resolves the
buffer-local watch state in the wrong buffer and silently drops the
arrival."
  (when fab--live-debounce
    (cancel-timer fab--live-debounce))
  (let ((buffer (current-buffer)))
    (setq fab--live-debounce
          (run-with-timer
           0.05 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (ignore-errors (fab--live-refresh)))))))))

(defun fab--live-refresh ()
  "Append responses missing from this buffer at the marker.
The renderer is the single source of shape: arrivals are sections
of a fresh `bus org' render whose ids the buffer does not already
carry — idempotent by construction, so a watch burst or a second
live view never duplicates.  No-ops to nothing when nothing is new."
  (when fab-live-mode
    (let* ((root (fab-buffer-root))
           (thread (fab-buffer-thread))
           (marker (fab--live-marker-position)))
      (when marker
        (let* ((rendered (fab--render root thread))
               (known (fab--post-ids-in-buffer))
               (missing (cl-remove-if
                         (lambda (section) (memq (car section) known))
                         (fab--render-post-sections rendered))))
          (when missing
            (save-excursion
              (goto-char marker)
              (insert (mapconcat #'cdr
                                 (sort missing (lambda (a b) (< (car a) (car b))))
                                 "\n")
                      "\n"))))))))

(provide 'fab)

;;; Status ⟜ one Haskell snapshot, one Org rendering

(defun fab--render-status (root)
  "Render ROOT through the unified `bus status --org' snapshot."
  (let ((result (apply #'fab--cli-stdin
                       ""
                       (list "status" "-r" root "--depth" "1" "--org"))))
    (unless (zerop (car result))
      (user-error "%s" (cdr result)))
    (cdr result)))

;;; Anchor tooltips and navigation ⟜ hover shows post excerpt, RET opens thread

(defun fab--post-excerpt (root id limit)
  "First LIMIT characters of post ID's body, via the Haskell-rendered
path (bus org --thread ID).  nil when the render fails or the post
is absent."
  (let* ((rendered (condition-case nil
                       (fab--render root id)
                     (user-error nil))))
    (when rendered
      (let* ((sections (fab--render-post-sections rendered))
             (section (cdr (assq id sections))))
        (when section
          (let* ((lines (split-string section "\n"))
                 (body (string-join (cdr lines) "\n"))
                 (clean (string-trim body)))
            (when (> (length clean) 0)
              (substring clean 0 (min limit (length clean))))))))))

(defvar fab-status-anchor-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'fab--anchor-open)
    (define-key map [mouse-2] #'fab--anchor-open)
    map)
  "Keymap for [#N] anchors in fab status buffers.")

(defun fab--anchor-open ()
  "Open the thread for the [#N] anchor at point via `fab-open',
using the current buffer's bus root.  The exact anchor id rides
on the anchor as a `fab-anchor-id' text property, so a row
carrying both a cursor and a post anchor opens the one point is
on, not the first bracket in the neighbourhood.  The created
thread buffer is displayed, not only created."
  (interactive)
  (let ((id (or (get-text-property (point) 'fab-anchor-id)
                (fab--id-at-point))))
    (unless id
      (user-error "No [#N] anchor at point"))
    (fab-open id (fab-buffer-root))
    (pop-to-buffer (format "*fab %d*" id))))

(defun fab--anchorize ()
  "Add tooltip and navigation properties to [#N] anchors in the
current buffer.  Tooltip: first 60 characters of the post body,
via the Haskell-rendered path.  Navigation: RET or mouse-2 opens
the thread through `fab-open'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[#\\([0-9]+\\)\\]" nil t)
      (let* ((id (string-to-number (match-string 1)))
             (start (match-beginning 0))
             (end (match-end 0))
             (map (let ((m (make-sparse-keymap)))
                    (set-keymap-parent m fab-status-anchor-map)
                    m)))
        (add-text-properties
         start end
         `(help-echo
           ,(lambda (_window buffer _pos)
              (with-current-buffer buffer
                (let ((root (fab-buffer-root)))
                  (or (fab--post-excerpt root id 60)
                      (format "post #%d" id)))))
           fab-anchor-id ,id
           keymap ,map
           follow-link t
           mouse-face highlight))))))

;;;###autoload
(defun fab-open-status (&optional root)
  "Open the unified `bus status --org' view in a temporary Org buffer.
The census header counts every membership circle, including retired and
non-fab participants.  [#N] anchors carry tooltips and navigation."
  (interactive)
  (let* ((root* (expand-file-name (or root fab-root)))
         (rendered (fab--render-status root*)))
    (with-current-buffer (get-buffer-create "*fab-status*")
      (erase-buffer)
      (insert (format "#+fab-root: %s\n\n" root*))
      (insert rendered)
      (unless (string-suffix-p "\n" (buffer-string))
        (insert "\n"))
      (org-mode)
      (fab--anchorize)
      (goto-char (point-min)))
    (when (called-interactively-p 'any)
      (pop-to-buffer "*fab-status*"))))

;;; Legacy shim (remove with card-composer-views migration)
;; card-composer-views.el still (require 'card-composer)
;; and calls card-composer--cli-stdin.  Both resolve here until its
;; selectors migrate to fab naming as the fast-follow.
(provide 'card-composer)

;;; fab.el ends here
