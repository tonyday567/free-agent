;;; free-agent-bus-deck.el --- free-agent-bus UI in Emacs -*- lexical-binding: t; -*-

;; A minimal Emacs surface for the free-agent-bus log.
;; Two buffers: *free-agent-bus-log* (tail) and *free-agent-bus-post* (write).
;;
;; Bind under your preferred leader prefix, e.g.:
;;   (map! :leader
;;         :prefix ("y" . "fab")
;;         :desc "send"  "RET" #'free-agent-bus-send
;;         :desc "send"  "SPC" #'free-agent-bus-send
;;         :desc "board" "b"   #'free-agent-bus-board
;;         :desc "post"  "p"   #'free-agent-bus-post
;;         :desc "log"   "l"   #'free-agent-bus-log
;;         :desc "deck"  "d"   #'free-agent-bus-deck)

;;; Commentary:

;; free-agent-bus-deck replaces the muster-deck UI for the new global-log
;; runtime. The log file remains the single source of truth; the log buffer is
;; a processed view that renders JSONL messages as `[id@ts] from: body' (or
;; `from: body' when timestamps are off). Multi-line posts read naturally
;; because the body is stored with real newlines. Posts are sent via the
;; `free-agent-bus' executable.
;;
;; Unlike muster-deck there is no daemon, no channel join/leave, and no cursor
;; state. A channel is just a filter on the `to' field of each post.
;;
;; Recipients are explicit, never inferred: the post buffer defaults to
;; broadcast as the visible list ["all"] (mode line shows FAB-Post[all]),
;; and a nil recipient list errors on send rather than silently fanning
;; out (loom/deck-design.md §feedback — nil-means-broadcast).
;;
;; Unparseable log lines are never dropped silently and never dumped raw
;; as if they were content: they render as ⟝-marked rows and an
;; increasing count is announced in *Messages* (count-check doctrine).

;;; Code:

(require 'filenotify)
(require 'json)
(require 'subr-x)

;;; Variables

(defgroup free-agent-bus-deck nil
  "Emacs surface for the free-agent-bus log."
  :group 'applications)

(defcustom free-agent-bus-executable "free-agent"
  "Executable used to scribe posts.
Invoked as: free-agent bus post --root ROOT, with the post JSON on stdin."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-root "~/pit/bus"
  "Root directory for the bus. The log lives at ROOT/log.jsonl.
The live house bus is ~/pit/bus (buff/bus.md §the root); the old
~/.config default silently pointed the deck at an empty tree
(loom/deck-design.md §what is actually wrong, finding 1)."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-channel "bus"
  "Legacy channel name. Kept for compatibility with older logs.
New posts default to broadcast (`[\"all\"]'); this name is added to the
log filter when `free-agent-bus-log-filter' is nil."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-post-to '("all")
  "Recipient list for the current or next post.
Defaults explicitly to `(\"all\")': broadcast is a visible, mode-line
shown choice, never a silent nil default (all three seats ranked
nil-means-broadcast the top defect — loom/deck-design.md §feedback).
If nil, sending errors instead of guessing: recipients must be resolved
and displayed before the post leaves (set with C-c C-a; empty input
selects broadcast explicitly).
The special list `[\"\"]' posts to the discard channel."
  :type '(choice (const :tag "Unresolved (error on send)" nil)
                 (repeat string))
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-name "deck"
  "Name to post as."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-post-buffer-name "*free-agent-bus-post*"
  "Name of the post buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-buffer-name "*free-agent-bus-log*"
  "Name of the log buffer."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-board-file "~/coffee/loom/board.md"
  "Path to the board markdown file.
This is a regular file; `free-agent-bus-board' simply opens it."
  :type 'string
  :group 'free-agent-bus-deck)

;;; Core helpers

(defun free-agent-bus--expand-root ()
  "Expand `free-agent-bus-root' to an absolute path."
  (expand-file-name free-agent-bus-root))

(defun free-agent-bus--log-file ()
  "Return path to the global log file."
  (expand-file-name "log.jsonl" (free-agent-bus--expand-root)))

(defun free-agent-bus--executable ()
  "Return the absolute path to the scribe executable, or its name if on PATH."
  (if (file-name-absolute-p free-agent-bus-executable)
      free-agent-bus-executable
    (or (executable-find free-agent-bus-executable)
        free-agent-bus-executable)))

;;; Sending

;;;###autoload
(defun free-agent-bus-send (begin end)
  "Send active region from BEGIN to END to the bus as `free-agent-bus-name'.
If no region is active, send the whole buffer, but only when in
`free-agent-bus-post-mode' to avoid accidentally posting source files.
The post is addressed to `free-agent-bus-post-to', which defaults
explicitly to broadcast (`to: [\"all\"]').  A nil recipient list is an
error, not a guess: recipients must be resolved and displayed before
the post leaves."
  (interactive "r")
  (let* ((use-region (use-region-p))
         (begin (if use-region begin (point-min)))
         (end (if use-region end (point-max))))
    (when (= begin end)
      (user-error "Nothing to send"))
    (unless (or use-region (derived-mode-p 'free-agent-bus-post-mode))
      (user-error "No active region; switch to free-agent-bus-post to send a full buffer"))
    (let ((text (string-trim (buffer-substring-no-properties begin end)))
          (coding-system-for-write 'utf-8))
      (when (string-empty-p text)
        (user-error "Nothing to send"))
      (let* ((root (free-agent-bus--expand-root))
             (recipients (and (boundp 'free-agent-bus-post-to)
                              free-agent-bus-post-to))
             (post `((from . ,free-agent-bus-name)
                     (to . ,(vconcat
                             (or recipients
                                 (user-error
                                  "Post recipients unresolved — set with C-c C-a (empty input = broadcast to all)"))))
                     (thread . [])
                     (body . ,text)))
             (json (json-encode post))
             (stdout-buffer (generate-new-buffer " *free-agent-bus-stdout*"))
             status)
        (unwind-protect
            (with-temp-buffer
              (insert json "\n")
              (setq status
                    (call-process-region
                     (point-min) (point-max)
                     (free-agent-bus--executable)
                     nil stdout-buffer nil
                     "bus" "post" "--root" root))
              (when (/= status 0)
                (error "free-agent-bus exited with code %d: %s"
                       status (with-current-buffer stdout-buffer (buffer-string))))
              (message "%s -> %s"
                       (string-trim (with-current-buffer stdout-buffer (buffer-string)))
                       recipients))
          (kill-buffer stdout-buffer))))
    (when (derived-mode-p 'free-agent-bus-post-mode)
      (erase-buffer))))

;;; Post buffer

(defvar free-agent-bus-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'free-agent-bus-send)
    (define-key map (kbd "C-c C-a") #'free-agent-bus-set-recipients)
    map)
  "Keymap for `free-agent-bus-post-mode'.")

;; Ensure bindings survive reloads (defvar does not overwrite an existing map).
(define-key free-agent-bus-post-mode-map (kbd "C-<return>") #'free-agent-bus-send)
(define-key free-agent-bus-post-mode-map (kbd "C-c C-a") #'free-agent-bus-set-recipients)

;;;###autoload
(defun free-agent-bus-set-recipients (recipients)
  "Set the recipient list for the current post.
RECIPIENTS is a comma or space separated string of names, e.g.
\"kimi, hermes\".  With empty input, sets broadcast explicitly
(`to: [\"all\"]') — broadcast is a visible choice, never an unresolved
nil (loom/deck-design.md §feedback)."
  (interactive
   (list (read-string
          "Recipients (empty = broadcast): "
          nil nil "")))
  (setq-local free-agent-bus-post-to
              (let ((trimmed (string-trim recipients)))
                (cond
                  ((string-empty-p trimmed) '("all"))        ; broadcast, explicit
                  ((string= trimmed "\"\"") '(""))           ; discard
                  (t (split-string trimmed "[ ,]+" t)))))
  (setq mode-name (free-agent-bus--post-mode-name))
  (force-mode-line-update)
  (message "Post recipients: %s"
           (if (equal free-agent-bus-post-to '("all"))
               "broadcast (all)"
             free-agent-bus-post-to)))

(defun free-agent-bus--post-mode-name ()
  "Return the mode name including current recipients."
  (if (and (boundp 'free-agent-bus-post-to) free-agent-bus-post-to)
      (format "FAB-Post[%s]" (mapconcat #'identity free-agent-bus-post-to ","))
    "FAB-Post[unresolved]"))

(define-derived-mode free-agent-bus-post-mode text-mode (free-agent-bus--post-mode-name)
  "Major mode for composing free-agent-bus posts."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local free-agent-bus-post-to (default-value 'free-agent-bus-post-to)))

;;;###autoload
(defun free-agent-bus-post ()
  "Open the free-agent-bus post buffer."
  (interactive)
  (pop-to-buffer free-agent-bus-post-buffer-name)
  (unless (derived-mode-p 'free-agent-bus-post-mode)
    (free-agent-bus-post-mode)))

;;; Board file

;;;###autoload
(defun free-agent-bus-board ()
  "Open the board file as a regular file."
  (interactive)
  (find-file (expand-file-name free-agent-bus-board-file)))

;;; Log buffer

(defcustom free-agent-bus-log-refresh-interval 1.0
  "Seconds between log refreshes when file-notify is unavailable."
  :type 'number
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-show-timestamps t
  "Whether to show timestamps in `*free-agent-bus-log*'."
  :type 'boolean
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-filter-choices '("tony" "kimi" "mucho" "singleton" "lungo" "oedipa")
  "Common directed-recipient filter choices for the log buffer.
Broadcasts (`to: [\"all\"]') are always shown."
  :type '(repeat string)
  :group 'free-agent-bus-deck)

(defvar-local free-agent-bus-log-filter nil
  "Current recipient filter for the log buffer.
A list of names; a post is shown if it is a broadcast (`to: [\"all\"]') or
if its `to' field contains any of them.  If nil, defaults to the user's
name plus the legacy channel name.")

(defvar free-agent-bus-log--timer nil
  "Fallback timer for periodic log refresh.")

(defvar free-agent-bus-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar free-agent-bus-log--parse-errors 0
  "Count of unparseable non-blank log lines in the last render.
When the count increases, `free-agent-bus-log-refresh' announces it in
the echo area — a count-check event, never a silent drop
(loom/deck-design.md §open questions; k2 post 365, gate point d).")

(defvar free-agent-bus-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'free-agent-bus-log-toggle-timestamps)
    (define-key map (kbd "f") #'free-agent-bus-log-set-filter)
    map)
  "Keymap for `free-agent-bus-log-mode'.")

(define-derived-mode free-agent-bus-log-mode text-mode "FreeAgentBus-Log"
  "Major mode for viewing free-agent-bus logs."
  (add-hook 'kill-buffer-hook #'free-agent-bus-log-stop-watch nil t))

(defun free-agent-bus-log--read-file ()
  "Read the global log file as UTF-8 text."
  (with-temp-buffer
    (insert-file-contents-literally (free-agent-bus--log-file))
    (set-buffer-file-coding-system 'utf-8)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (buffer-string)))

(defun free-agent-bus-log--format-line (line)
  "Format a raw log LINE for display.
Accepts stamped JSONL (`id', `ts', `from', `to', `thread', `body').
Lines not addressed to the current `free-agent-bus-channel' are ignored.
Blank lines (separators, trailing newline) are skipped silently.
Unparseable lines increment `free-agent-bus-log--parse-errors' and
render as a visible ⟝-marked row — never returned raw as if they were
content (loom/deck-design.md §what is actually wrong, finding 3)."
  (let ((parsed (ignore-errors (json-read-from-string line))))
    (cond
     ((string-blank-p line) nil)
     ((null parsed)
      ;; Count-check event: visible row in the buffer, counted for *Messages*
      ;; by `free-agent-bus-log-refresh'.  The raw line stays in the log file.
      (setq free-agent-bus-log--parse-errors
            (1+ free-agent-bus-log--parse-errors))
      (format "⟝ UNPARSEABLE (raw line kept in %s): %.160s"
              (free-agent-bus--log-file) line))
     (t
      (ignore-errors
        (let* ((obj parsed)
               (id (cdr (assoc 'id obj)))
               (ts (cdr (assoc 'ts obj)))
               (from (cdr (assoc 'from obj)))
               (to (cdr (assoc 'to obj)))
               (body (cdr (assoc 'body obj))))
          (when (and id ts from body
                     (not (or (equal to [""]) (seq-empty-p to)))
                     (or (seq-find (lambda (c) (equal c "all")) to)
                         (let ((filter (or free-agent-bus-log-filter
                                           (list free-agent-bus-name
                                                 free-agent-bus-channel))))
                           (seq-find (lambda (c) (member c filter)) to))))
            (if free-agent-bus-log-show-timestamps
                (format "[%s@%s] %s: %s" id ts from body)
              (format "%s: %s" from body)))))))))

(defun free-agent-bus-log-refresh (&optional force)
  "Refresh the log buffer from the file, preserving scroll position.
When FORCE is non-nil, re-render even if the file content appears
unchanged. Keeps the tail in view for any window that was already at
the end of the log.  Parse-error count-check: when the unparseable-line
count increases versus the previous render, the increase is announced
in the echo area (*Messages*)."
  (when-let ((buf (get-buffer free-agent-bus-log-buffer-name)))
    (with-current-buffer buf
      (let ((previous-errors free-agent-bus-log--parse-errors))
        (setq free-agent-bus-log--parse-errors 0)
        (let* ((inhibit-read-only t)
             (raw (free-agent-bus-log--read-file))
             (contents (mapconcat #'identity
                                  (delq nil (mapcar #'free-agent-bus-log--format-line
                                                    (split-string raw "\n")))
                                  "\n")))
          (when (> free-agent-bus-log--parse-errors previous-errors)
            (message "free-agent-bus: %d unparseable log line(s), marked ⟝ — raw line(s) in %s"
                     free-agent-bus-log--parse-errors (free-agent-bus--log-file)))
          (when (or force (not (string= contents (buffer-string))))
          (let ((window-states
                 (mapcar (lambda (w)
                           (let ((pt (window-point w)))
                             (list w pt (= pt (point-max)))))
                         (get-buffer-window-list buf nil t))))
            (erase-buffer)
            (insert contents)
            (dolist (state window-states)
              (let* ((w (nth 0 state))
                     (old-pt (nth 1 state))
                     (was-at-end (nth 2 state))
                     (new-pt (if was-at-end (point-max) (min old-pt (point-max)))))
                  (set-window-point w new-pt))))))))))

(defun free-agent-bus-log--refresh-soon ()
  "Schedule a log refresh after a short debounce window.
Multiple file-notify events in quick succession collapse into one refresh."
  (when free-agent-bus-log--timer
    (cancel-timer free-agent-bus-log--timer))
  (setq free-agent-bus-log--timer
        (run-with-timer 0.05 nil #'free-agent-bus-log-refresh)))

(defun free-agent-bus-log-toggle-timestamps ()
  "Toggle timestamp display in `*free-agent-bus-log*' and force a re-render."
  (interactive)
  (setq free-agent-bus-log-show-timestamps (not free-agent-bus-log-show-timestamps))
  (message "free-agent-bus log timestamps %s"
           (if free-agent-bus-log-show-timestamps "on" "off"))
  (free-agent-bus-log-refresh t))

;;;###autoload
(defun free-agent-bus-log-set-filter (filter)
  "Set the directed-recipient filter for the current log buffer.
FILTER is a comma or space separated list of names. Broadcasts (`to: []')
are always shown; directed posts are shown only if their `to' field
contains one of the filter names. Common choices are offered via completion."
  (interactive
   (let* ((default (mapconcat #'identity
                              (or free-agent-bus-log-filter
                                  (list free-agent-bus-name
                                        free-agent-bus-channel))
                              ","))
          (choice (completing-read
                   (format-prompt "Log filter" default)
                   (seq-uniq (append free-agent-bus-log-filter-choices
                                     (list free-agent-bus-name)))
                   nil nil nil nil default)))
     (list (split-string (string-trim choice) "[ ,]+" t))))
  (setq-local free-agent-bus-log-filter
              (if (stringp filter) (list filter) filter))
  (message "Log filter: %s" free-agent-bus-log-filter)
  (free-agent-bus-log-refresh t))

(defun free-agent-bus-log-start-watch ()
  "Start watching the log file.
Prefer file-notify events; fall back to a polling timer if unavailable."
  (free-agent-bus-log-stop-watch)
  (condition-case nil
      (setq free-agent-bus-log--file-watch
            (file-notify-add-watch (free-agent-bus--log-file)
                                   '(change)
                                   (lambda (_event)
                                     (free-agent-bus-log--refresh-soon))))
    (error
     (setq free-agent-bus-log--timer
           (run-with-timer 0 free-agent-bus-log-refresh-interval #'free-agent-bus-log-refresh)))))

(defun free-agent-bus-log-stop-watch ()
  "Stop watching the log file."
  (when free-agent-bus-log--timer
    (cancel-timer free-agent-bus-log--timer)
    (setq free-agent-bus-log--timer nil))
  (when free-agent-bus-log--file-watch
    (file-notify-rm-watch free-agent-bus-log--file-watch)
    (setq free-agent-bus-log--file-watch nil)))

;;;###autoload
(defun free-agent-bus-log ()
  "Open the free-agent-bus log buffer and start tailing it."
  (interactive)
  (let ((log-file (free-agent-bus--log-file)))
    (unless (file-readable-p log-file)
      (user-error "Log file not found: %s" log-file))
    (let ((buf (get-buffer-create free-agent-bus-log-buffer-name)))
      (with-current-buffer buf
        (free-agent-bus-log-mode)
        (setq buffer-read-only t)
        (free-agent-bus-log-start-watch)
        (free-agent-bus-log-refresh)
        (goto-char (point-max)))
      (pop-to-buffer buf))))

;;; Deck

;;;###autoload
(defun free-agent-bus-deck ()
  "Open the full deck: log on top, post across the bottom."
  (interactive)
  (delete-other-windows)
  ;; Split into top (log) and bottom (post), so post spans full width.
  (split-window-below)
  (let* ((top-win (selected-window))
         (bottom-win (next-window))
         (log-buf (get-buffer-create free-agent-bus-log-buffer-name))
         (post-buf (get-buffer-create free-agent-bus-post-buffer-name)))
    ;; Top: log
    (set-window-buffer top-win log-buf)
    (with-current-buffer log-buf
      (free-agent-bus-log-mode)
      (setq buffer-read-only t)
      (free-agent-bus-log-start-watch)
      (free-agent-bus-log-refresh)
      (goto-char (point-max)))
    (set-window-point top-win (with-current-buffer log-buf (point-max)))
    ;; Bottom: post
    (select-window bottom-win)
    (set-window-buffer bottom-win post-buf)
    (with-current-buffer post-buf
      (unless (derived-mode-p 'free-agent-bus-post-mode)
        (free-agent-bus-post-mode)))
    (select-window top-win)))

;;;###autoload
(defun free-agent-bus-deck-quit ()
  "Close free-agent-bus deck buffers and stop the log watcher."
  (interactive)
  (free-agent-bus-log-stop-watch)
  (dolist (buf (list free-agent-bus-post-buffer-name
                     free-agent-bus-log-buffer-name))
    (when-let ((b (get-buffer buf)))
      (kill-buffer b))))

;; Doom leader binding, if available. Keeps the deck self-contained so
;; `SPC y f' works without a full Doom reload after loading this file.
(when (boundp 'doom-leader-map)
  (define-key doom-leader-map "yf" #'free-agent-bus-log-set-filter))

(provide 'free-agent-bus-deck)
;;; free-agent-bus-deck.el ends here
