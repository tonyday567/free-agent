;;; free-agent-bus-deck.el --- free-agent-bus UI in Emacs -*- lexical-binding: t; -*-

;; A minimal Emacs surface for the free-agent-bus log.
;; Two buffers: *free-agent-bus-log* (tail) and *free-agent-bus-post* (write).
;;
;; Bind under your preferred leader prefix, e.g.:
;;   (map! :leader
;;         :prefix ("y" . "fab")
;;         :desc "send"  "RET" #'free-agent-bus-send
;;         :desc "send"  "SPC" #'free-agent-bus-send
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

;;; Code:

(require 'filenotify)
(require 'json)

;;; Variables

(defgroup free-agent-bus-deck nil
  "Emacs surface for the free-agent-bus log."
  :group 'applications)

(defcustom free-agent-bus-executable "free-agent-bus"
  "Executable used to scribe posts."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-root "~/.config/free-agent-bus"
  "Root directory for the bus. The log lives at ROOT/log.jsonl."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-channel "bus"
  "Default channel. Used as the single `to' recipient when posting,
and as a display filter in the log buffer."
  :type 'string
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
The post is addressed to `free-agent-bus-channel'."
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
             (post `((from . ,free-agent-bus-name)
                     (to . [,free-agent-bus-channel])
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
                     nil stdout-buffer nil root))
              (when (/= status 0)
                (error "free-agent-bus exited with code %d: %s"
                       status (with-current-buffer stdout-buffer (buffer-string))))
              (message "%s" (string-trim
                             (with-current-buffer stdout-buffer (buffer-string)))))
          (kill-buffer stdout-buffer))))
    (when (derived-mode-p 'free-agent-bus-post-mode)
      (erase-buffer))))

;;; Post buffer

(defvar free-agent-bus-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'free-agent-bus-send)
    map)
  "Keymap for `free-agent-bus-post-mode'.")

(define-derived-mode free-agent-bus-post-mode text-mode "FreeAgentBus-Post"
  "Major mode for composing free-agent-bus posts."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;;###autoload
(defun free-agent-bus-post ()
  "Open the free-agent-bus post buffer."
  (interactive)
  (pop-to-buffer free-agent-bus-post-buffer-name)
  (unless (derived-mode-p 'free-agent-bus-post-mode)
    (free-agent-bus-post-mode)))

;;; Log buffer

(defcustom free-agent-bus-log-refresh-interval 1.0
  "Seconds between log refreshes when file-notify is unavailable."
  :type 'number
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-show-timestamps t
  "Whether to show timestamps in `*free-agent-bus-log*'."
  :type 'boolean
  :group 'free-agent-bus-deck)

(defvar free-agent-bus-log--timer nil
  "Fallback timer for periodic log refresh.")

(defvar free-agent-bus-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar free-agent-bus-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'free-agent-bus-log-toggle-timestamps)
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
Unparseable lines are returned as-is."
  (let ((formatted (ignore-errors
                     (let* ((obj (json-read-from-string line))
                            (id (cdr (assoc 'id obj)))
                            (ts (cdr (assoc 'ts obj)))
                            (from (cdr (assoc 'from obj)))
                            (to (cdr (assoc 'to obj)))
                            (body (cdr (assoc 'body obj))))
                       (when (and id ts from body
                                  (seq-find (lambda (c) (string= c free-agent-bus-channel)) to))
                         (if free-agent-bus-log-show-timestamps
                             (format "[%s@%s] %s: %s" id ts from body)
                           (format "%s: %s" from body)))))))
    (if formatted formatted
      ;; Return nil for parsed-but-non-matching posts so they are filtered out.
      ;; Unparseable lines are returned as-is so the user sees the raw file.
      (unless (ignore-errors (json-read-from-string line)) line))))

(defun free-agent-bus-log-refresh (&optional force)
  "Refresh the log buffer from the file, preserving scroll position.
When FORCE is non-nil, re-render even if the file content appears
unchanged. Keeps the tail in view for any window that was already at
the end of the log."
  (when-let ((buf (get-buffer free-agent-bus-log-buffer-name)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (raw (free-agent-bus-log--read-file))
             (contents (mapconcat #'identity
                                  (delq nil (mapcar #'free-agent-bus-log--format-line
                                                    (split-string raw "\n")))
                                  "\n")))
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
                (set-window-point w new-pt)))))))))

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

(provide 'free-agent-bus-deck)
;;; free-agent-bus-deck.el ends here
