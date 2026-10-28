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
;;   *free-agent-bus-post*    composition buffer           (bus say)
;;
;; Cascade depth is explicit everywhere: 0 dot, 1 one-line excerpt, 2 verbatim.
;; No view advances an agent delivery cursor and no view invokes a model.
;;
;; Audience: a post with `to: []' is public and is always shown. There is no
;; broadcast token: the retired `"all"' convention is gone from composition and
;; from the filter, and `free-agent-bus-post-to' has no implicit default.
;;
;; Keys (Doom leader; also bound inside the buffer maps):
;;   SPC y RET / SPC y SPC   send region (or whole post buffer)
;;   SPC y p                 composition buffer
;;   SPC y l                 log tail
;;   SPC y s                 live status
;;   SPC y r                 READY tasks
;;   SPC y t                 thread at point (or prompt for a post id)
;;   SPC y f                 set the audience filter (empty = show everything)
;;   SPC y b                 open the board file
;;   SPC y d                 deck: status + log + compose in one frame
;;   SPC y q                 close deck buffers
;; In *free-agent-bus-log* and the view buffers:
;;   RET  open the thread of the post at point      TAB  cycle cascade depth
;;   g    refresh        q  quit window      t/f  timestamps / filter (log only)

;;; Code:

(require 'filenotify)
(require 'json)
(require 'seq)
(require 'subr-x)

;;; Variables

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

(defcustom free-agent-bus-name "tony"
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

(defcustom free-agent-bus-board-file "~/coffee/loom/board.md"
  "Path to the board markdown file, opened as a regular file.
The loom board card has since been retired; set this to whatever card now
holds the runner's board, or use `SPC y r' for the READY view instead."
  :type 'string
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-refresh-interval 1.0
  "Seconds between log refreshes when file-notify is unavailable."
  :type 'number
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-auto-refresh-interval 5.0
  "Seconds between re-runs of a visible CLI view (status, READY, thread).
The CLI reads the log and writes no cursor; a paused view costs nothing."
  :type 'number
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-show-timestamps t
  "Whether to show post ids and timestamps in the log tail."
  :type 'boolean
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-depth 1
  "Cascade depth for the log tail: 0 dot, 1 one-line excerpt, 2 verbatim."
  :type '(choice (const 0) (const 1) (const 2))
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-log-filter-choices '("tony" "moka" "mucho" "ristretto" "stinky" "doppio" "allegro")
  "Common audience filter choices offered by `free-agent-bus-log-set-filter'.
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

(defun free-agent-bus--cli (subcommand &rest args)
  "Run `free-agent bus SUBCOMMAND --root ROOT ARGS...'.
Return (STATUS . OUTPUT).  Never signals on a non-zero exit: a failing
command stays visible to the runner instead of being hidden by an error."
  (free-agent-bus--check-root)
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (let ((status (apply #'call-process (free-agent-bus--executable) nil t nil
                           "bus" subcommand "--root" (free-agent-bus--expand-root) args)))
        (cons status (string-trim-right (buffer-string)))))))

(defun free-agent-bus--cli-stdin (input subcommand &rest args)
  "Run the CLI with INPUT on stdin.  Return (STATUS . OUTPUT)."
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert input)
      (let ((status (apply #'call-process-region (point-min) (point-max)
                           (free-agent-bus--executable) nil t nil
                           "bus" subcommand "--root" (free-agent-bus--expand-root) args)))
        (cons status (string-trim-right (buffer-string)))))))

(defun free-agent-bus--context-line ()
  "Identity and freshness line shared by every view."
  (format "root %s · posting as %s · %s"
          (free-agent-bus--expand-root)
          free-agent-bus-name
          (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun free-agent-bus--audience-string (to)
  "Render a post's TO field (a vector or list) as an audience string."
  (let ((names (append to nil)))
    (if names (mapconcat #'identity names ",") "public")))

(defun free-agent-bus--id-at-point ()
  "Return the post id on the current line, or nil.
Understands the log tail (`[id@ts]'), CLI views (`id from→to'), the thread
view (raw JSONL), and text mentioning `id=<n>'."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "\\[\\([0-9]+\\)@") (string-to-number (match-string 1)))
     ((looking-at "{\"id\":\\([0-9]+\\)") (string-to-number (match-string 1)))
     ((looking-at "\\([0-9]+\\)[ \t]") (string-to-number (match-string 1)))
     ((re-search-forward "id=\\([0-9]+\\)" (line-end-position) t)
      (string-to-number (match-string 1))))))

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

;;; Composition

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

(defun free-agent-bus--post-mode-refresh ()
  "Update the mode line after an audience or ancestry change."
  (setq mode-name (free-agent-bus--post-mode-name))
  (force-mode-line-update))

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

;;;###autoload
(defun free-agent-bus-set-recipients (recipients)
  "Set the explicit audience for this post.
RECIPIENTS is a comma or space separated list of names.  The single word
`public' addresses the post to everyone (`to: []').  Empty input clears the
audience; a post needs an audience or public before it will send."
  (interactive (list (read-string "Recipients (comma separated, or 'public'): "
                                  nil nil
                                  (cond (free-agent-bus-post-public "public")
                                        (free-agent-bus-post-to
                                         (mapconcat #'identity free-agent-bus-post-to ","))
                                        (t "")))))
  (let ((trimmed (string-trim recipients)))
    (cond
     ((string-empty-p trimmed)
      (setq free-agent-bus-post-public nil)
      (setq free-agent-bus-post-to nil))
     ((string= (downcase trimmed) "public")
      (setq free-agent-bus-post-public t)
      (setq free-agent-bus-post-to nil))
     (t
      (setq free-agent-bus-post-public nil)
      (setq free-agent-bus-post-to (split-string trimmed "[ ,]+" t)))))
  (when (derived-mode-p 'free-agent-bus-post-mode)
    (free-agent-bus--post-mode-refresh))
  (message "Audience: %s" (free-agent-bus--current-audience)))

(defun free-agent-bus-set-thread (id)
  "Set the post this composition answers.
ID is a post id; empty input clears the ancestry.  The post id is the task
id, so replying into a task's thread is how a CLAIM or DONE is filed."
  (interactive (list (let ((input (read-string "Thread (post id, empty for a root post): "
                                              nil nil
                                              (if free-agent-bus-post-thread
                                                  (number-to-string free-agent-bus-post-thread)
                                                ""))))
                       (if (string-empty-p (string-trim input))
                           nil
                         (string-to-number (string-trim input))))))
  (setq free-agent-bus-post-thread id)
  (when (derived-mode-p 'free-agent-bus-post-mode)
    (free-agent-bus--post-mode-refresh))
  (message "Thread: %s" (or free-agent-bus-post-thread "root post")))

(defun free-agent-bus--current-audience ()
  "Audience of the next post as a string, for the mode line and messages."
  (cond (free-agent-bus-post-public "public")
        (free-agent-bus-post-to (mapconcat #'identity free-agent-bus-post-to ","))
        (t "none")))

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
        (user-error "No audience: set recipients first (C-c C-a) or 'public'"))
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
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "%s · depth %s\n%s\n\n"
                                (plist-get spec :title) depth
                                (free-agent-bus--context-line))
                        'face 'bold))
    (insert (if (string-empty-p output)
                (propertize "(no rows)\n" 'face 'shadow)
              (concat output "\n")))
    (when (/= status 0)
      (insert (propertize (format "\n⛔ free-agent bus %s exited %d\n" sub status) 'face 'error)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun free-agent-bus-cycle-depth ()
  "Cycle the cascade depth of the current view: 0 → 1 → 2 → 0."
  (interactive)
  (let* ((current (cond ((derived-mode-p 'free-agent-bus-log-mode) free-agent-bus-log-depth)
                        ((derived-mode-p 'free-agent-bus-view-mode) free-agent-bus-view-depth)
                        (t 1)))
         (next (cond ((= current 0) 1) ((= current 1) 2) (t 0))))
    (cond ((derived-mode-p 'free-agent-bus-log-mode)
           (setq free-agent-bus-log-depth next)
           (free-agent-bus-log-refresh t)
           (message "Depth %s" next))
          ((derived-mode-p 'free-agent-bus-view-mode)
           (setq free-agent-bus-view-depth next)
           (free-agent-bus--view-refresh)
           (message "Depth %s" next))
          (t (message "Depth applies inside the log and view buffers")))))

(defvar free-agent-bus-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'free-agent-bus-thread-at-point)
    (define-key map (kbd "<tab>") #'free-agent-bus-cycle-depth)
    (define-key map (kbd "t") #'free-agent-bus-thread-at-point)
    map)
  "Keymap for CLI-backed bus views.")

(define-key free-agent-bus-view-mode-map (kbd "RET") #'free-agent-bus-thread-at-point)
(define-key free-agent-bus-view-mode-map (kbd "<tab>") #'free-agent-bus-cycle-depth)

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

;;;###autoload
(defun free-agent-bus-status ()
  "Open the live status view: freshness, seats, heartbeat, cursor, last post.
Seat dots describe loop liveness, not the quality of work or whether it was
accepted.  Uses `bus status --live'; no cursor is written."
  (interactive)
  (free-agent-bus--open-view free-agent-bus-status-buffer-name
                             "bus status (live)" "status" '("--live") 1))

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
Depth 2 shows the verbatim log lines, so the view is evidence, not a
retelling: a task's body, its claim, and its DONE read in log order."
  (interactive (list (or (free-agent-bus--id-at-point)
                         (string-to-number (read-string "Thread id (post id): ")))))
  (unless (integerp id)
    (user-error "No post id at point and none given"))
  (free-agent-bus--open-view free-agent-bus-thread-buffer-name
                             (format "thread %s" id) "read"
                             (list "--thread" (number-to-string id)) 2))

;;;###autoload
(defun free-agent-bus-thread-at-point ()
  "Open the thread of the post at point, or offer to open any thread."
  (interactive)
  (let ((id (free-agent-bus--id-at-point)))
    (if id
        (free-agent-bus-thread id)
      (call-interactively #'free-agent-bus-thread))))

;;; Auto refresh (visible views only; never a model, never a cursor)

(defvar free-agent-bus--auto-timer nil
  "Timer re-running visible CLI views.")

(defun free-agent-bus--auto-refresh-buffers ()
  "Refresh every visible CLI view; stop the timer when none is visible."
  (let ((visible nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and free-agent-bus--view-spec (get-buffer-window buf))
          (setq visible t)
          (ignore-errors (free-agent-bus--view-refresh)))))
    (unless visible
      (free-agent-bus--stop-auto-refresh))))

(defun free-agent-bus--start-auto-refresh ()
  "Start the visible-view refresh timer, if it is not already running."
  (unless free-agent-bus--auto-timer
    (setq free-agent-bus--auto-timer
          (run-with-timer free-agent-bus-auto-refresh-interval
                          free-agent-bus-auto-refresh-interval
                          #'free-agent-bus--auto-refresh-buffers))))

(defun free-agent-bus--stop-auto-refresh ()
  "Stop the visible-view refresh timer."
  (when free-agent-bus--auto-timer
    (cancel-timer free-agent-bus--auto-timer)
    (setq free-agent-bus--auto-timer nil)))

;;; Log tail

(defvar free-agent-bus-log--timer nil
  "Fallback timer for periodic log refresh.")

(defvar free-agent-bus-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar free-agent-bus-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'free-agent-bus-log-toggle-timestamps)
    (define-key map (kbd "f") #'free-agent-bus-log-set-filter)
    (define-key map (kbd "RET") #'free-agent-bus-thread-at-point)
    (define-key map (kbd "<tab>") #'free-agent-bus-cycle-depth)
    map)
  "Keymap for `free-agent-bus-log-mode'.")

(define-key free-agent-bus-log-mode-map (kbd "RET") #'free-agent-bus-thread-at-point)
(define-key free-agent-bus-log-mode-map (kbd "<tab>") #'free-agent-bus-cycle-depth)

(define-derived-mode free-agent-bus-log-mode text-mode "FreeAgentBus-Log"
  "Major mode for the bus log tail.
\\{free-agent-bus-log-mode-map}"
  (add-hook 'kill-buffer-hook #'free-agent-bus-log-stop-watch nil t))

(defun free-agent-bus-log--read-file ()
  "Read the bus log file as UTF-8 text."
  (with-temp-buffer
    (insert-file-contents-literally (free-agent-bus--log-file))
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

(defun free-agent-bus-log--format-line (line)
  "Format one raw log LINE at the buffer's cascade depth.
0 = dot, 1 = one-line excerpt, 2 = verbatim body.  Unparseable lines are
returned as-is so the runner sees exactly what is in the file, and a post
whose audience does not match is dropped."
  (let ((obj (ignore-errors (json-read-from-string line))))
    (cond
     ((null obj) line)
     ((not (free-agent-bus-log--shows-p (alist-get 'to obj))) nil)
     (t
      (let* ((id (alist-get 'id obj))
             (ts (alist-get 'ts obj))
             (from (alist-get 'from obj))
             (audience (free-agent-bus--audience-string (alist-get 'to obj)))
             (thread (append (alist-get 'thread obj) nil))
             (body (or (alist-get 'body obj) ""))
             (stamped (if free-agent-bus-log-show-timestamps
                          (format "[%s@%s]" id ts)
                        (format "[%s]" id)))
             (depth free-agent-bus-log-depth))
        (pcase depth
          (0 (format "%s %s→%s%s" stamped from audience
                     (if thread " ↩" "")))
          (1 (format "%s %s→%s%s %s" stamped from audience
                     (if thread " ↩" "")
                     (car (split-string body "\n"))))
          (_ (format "%s %s→%s%s\n%s"
                     stamped from audience
                     (if thread " ↩" "")
                     (mapconcat (lambda (l) (concat "│ " l))
                                (split-string body "\n") "\n")))))))))

(defun free-agent-bus-log-refresh (&optional force)
  "Refresh the log buffer from the file, preserving scroll position.
When FORCE is non-nil, re-render even when the rendered text is unchanged."
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
  "Schedule a log refresh after a short debounce window."
  (when free-agent-bus-log--timer
    (cancel-timer free-agent-bus-log--timer))
  (setq free-agent-bus-log--timer
        (run-with-timer 0.05 nil #'free-agent-bus-log-refresh)))

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
      (unless (derived-mode-p 'free-agent-bus-view-mode)
        (free-agent-bus-view-mode))
      (setq-local free-agent-bus--view-spec
                  (list :title "bus status (live)" :subcommand "status"
                        :args '("--live") :base-depth 1))
      (setq-local free-agent-bus-view-depth 1)
      (free-agent-bus--view-refresh))
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
        (select-window main)))
    (free-agent-bus--start-auto-refresh)))

;;;###autoload
(defun free-agent-bus-deck-quit ()
  "Close the deck buffers, stop the log watcher, and stop view refresh."
  (interactive)
  (free-agent-bus-log-stop-watch)
  (free-agent-bus--stop-auto-refresh)
  (dolist (buf (list free-agent-bus-post-buffer-name
                     free-agent-bus-log-buffer-name
                     free-agent-bus-status-buffer-name
                     free-agent-bus-ready-buffer-name
                     free-agent-bus-thread-buffer-name))
    (when-let ((b (get-buffer buf)))
      (kill-buffer b))))

;; Leader bindings, self-contained: the deck binds its own keys whenever
;; `doom-leader-map' exists, so no Doom config edit is needed for a hot-load
;; or a restart.  A Doom `map!' block only adds which-key descriptions.
(when (boundp 'doom-leader-map)
  (dolist (pair '(("yf" . free-agent-bus-log-set-filter)
                  ("ys" . free-agent-bus-status)
                  ("yr" . free-agent-bus-ready)
                  ("yt" . free-agent-bus-thread-at-point)))
    (define-key doom-leader-map (kbd (car pair)) (cdr pair))))

(provide 'free-agent-bus-deck)
;;; free-agent-bus-deck.el ends here
