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
;;
;; Render shape (loom/deck-design.md §shape that follows): the log buffer
;; is org so folding is free.  A channel is a participant set — a directed
;; post's channel is FROM∪TO sorted, so both directions of a pair share
;; one foldable section; anything with "all" in TO is the commons
;; section, one circle per seat, house palette.  A post is a card
;; fragment: `from ⟜ body', replies nested one headline level under their
;; parent when it is in the same section, else marked ↩ at section level.
;; Channels fold on TAB; after each refresh every channel is folded
;; except the one holding the newest post.

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

(defcustom free-agent-bus-log-filter-choices
  '("runner" "k1" "k2" "k3" "mucho" "singleton" "lungo" "oedipa" "all")
  "Common directed-recipient filter choices for the log buffer —
the live roster plus the commons.  Broadcasts (`to: [\"all\"]') are
always shown regardless of the filter."
  :type '(repeat string)
  :group 'free-agent-bus-deck)

(defvar-local free-agent-bus-log-filter nil
  "Current recipient filter for the log buffer, buffer-local.
A list of names; a post is shown if it is a broadcast (`to: [\"all\"]')
or if its `to' field contains any of them.  If nil when
`free-agent-bus-log-mode' starts, it is initialized to
\(list free-agent-bus-name \"all\") — your mail plus the commons.
A global setq does NOT reach an existing log buffer (k2, wave 4:
the live buffer rode a stale c61158c-era filter and silently hid
directed mail); set it buffer-locally or via \\[free-agent-bus-log-set-filter].")

(defvar free-agent-bus-log--timer nil
  "Fallback timer for periodic log refresh.")

(defvar free-agent-bus-log--file-watch nil
  "File-notify descriptor for the log file.")

(defvar free-agent-bus-log--parse-errors 0
  "Count of unparseable non-blank log lines in the last render.
When the count increases, `free-agent-bus-log-refresh' announces it in
the echo area — a count-check event, never a silent drop
(loom/deck-design.md §open questions; k2 post 365, gate point d).")

;;; Faces and seat colors (house palette, stuff/house-style.md)

(defface free-agent-bus-mark-face
  '((t :weight bold))
  "Face for coordination marks (🟣🟡🟢🔴🔵🚩✓↩⟜⟝) in log bodies."
  :group 'free-agent-bus-deck)

(defface free-agent-bus-meta-face
  '((t :inherit shadow))
  "Face for the trailing id@ts tag on a rendered post."
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-seat-colors
  '(("all" . "#5b5ba0")                            ; indigo-hi — the commons
    ("runner" . "#D98A3A") ("tony" . "#D98A3A")     ; amber — the human
    ("k1" . "#e070a8") ("k2" . "#e070a8") ("k3" . "#e070a8") ; magenta-hi — kimis
    ("mucho" . "#3fa47e")                           ; forest-hi
    ("singleton" . "#6bb898")                       ; teal-hi
    ("lungo" . "#E07A9E")                           ; rose
    ("oedipa" . "#8FB83A"))                         ; chartreuse
  "Seat name to circle color. Colors from stuff/house-style.md
(dark-mode -hi values); unknown seats get
`free-agent-bus-unknown-seat-color' rather than a guess."
  :type '(alist :key-type string :value-type string)
  :group 'free-agent-bus-deck)

(defcustom free-agent-bus-unknown-seat-color "#777777"
  "Circle color for seats with no `free-agent-bus-seat-colors' entry."
  :type 'string
  :group 'free-agent-bus-deck)

(defun free-agent-bus--seat-color (name)
  "Return the circle color for seat NAME."
  (or (cdr (assoc name free-agent-bus-seat-colors))
      free-agent-bus-unknown-seat-color))

(defun free-agent-bus--circle (name)
  "Return a propertized ● for seat NAME."
  (propertize "●" 'face `(:foreground ,(free-agent-bus--seat-color name))))

(defvar free-agent-bus-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'free-agent-bus-log-toggle-timestamps)
    (define-key map (kbd "f") #'free-agent-bus-log-set-filter)
    map)
  "Keymap for `free-agent-bus-log-mode'.")

(define-derived-mode free-agent-bus-log-mode org-mode "FAB-Log"
  "Major mode for viewing free-agent-bus logs: an org fold over the log.
TAB folds channels and threads; `t' toggles timestamps, `f' sets the
recipient filter. Coordination marks render bold."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local org-startup-folded nil)
  (unless free-agent-bus-log-filter
    ;; Buffer-local: a global setq never reaches this buffer (k2, wave 4 —
    ;; a stale global filter silently hid directed mail in the live deck).
    (setq-local free-agent-bus-log-filter (list free-agent-bus-name "all")))
  (font-lock-add-keywords
   nil '(("[🟣🟡🟢🔴🔵🚩✓↩⟜⟝]" 0 'free-agent-bus-mark-face prepend)))
  (add-hook 'kill-buffer-hook #'free-agent-bus-log-stop-watch nil t))

(defun free-agent-bus-log--read-file ()
  "Read the global log file as UTF-8 text."
  (with-temp-buffer
    (insert-file-contents-literally (free-agent-bus--log-file))
    (set-buffer-file-coding-system 'utf-8)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (buffer-string)))

(defun free-agent-bus-log--complete-lines (raw)
  "Split RAW log text into newline-terminated lines only.
A trailing line without its terminating newline is a possibly-torn
in-progress append: hold it out of this render.  The log is append-only
and the whole file is re-read on every refresh, so the completed line
reaches the parser on the next pass — accumulation, one refresh deep,
with no cross-refresh state to drift (k2, wave 2 target 1: chunk-split).
Only complete lines can be parsed, which keeps the parse-error
count-check honest: a torn tail is never counted as corruption."
  (let ((lines (split-string raw "\n")))
    (if (string-suffix-p "\n" raw)
        lines
      (seq-take lines (max 0 (1- (length lines)))))))

(defun free-agent-bus-log--parse-post (line)
  "Parse one RAW log LINE into a plist (:id :ts :from :to :thread :body).
Returns nil for blank or filtered-out lines; increments
`free-agent-bus-log--parse-errors' and returns :unparseable for corrupt
lines (loom/deck-design.md §what is actually wrong, finding 3)."
  (cond
   ((string-blank-p line) nil)
   (t
    (let ((parsed (ignore-errors (json-read-from-string line))))
      (cond
       ((null parsed)
        ;; Count-check event: counted for *Messages* by
        ;; `free-agent-bus-log-refresh'; the raw line stays in the log file.
        (setq free-agent-bus-log--parse-errors
              (1+ free-agent-bus-log--parse-errors))
        :unparseable)
       (t
        (ignore-errors
          (let* ((id (cdr (assoc 'id parsed)))
                 (ts (cdr (assoc 'ts parsed)))
                 (from (cdr (assoc 'from parsed)))
                 (to (cdr (assoc 'to parsed)))
                 (thread (cdr (assoc 'thread parsed)))
                 (body (cdr (assoc 'body parsed))))
            (when (and id ts from body
                       (not (or (equal to [""]) (seq-empty-p to)))
                       (or (seq-find (lambda (c) (equal c "all")) to)
                           (let ((filter (or free-agent-bus-log-filter
                                             (list free-agent-bus-name "all"))))
                             (seq-find (lambda (c) (member c filter)) to))))
              (list :id id :ts ts :from from
                    :to (append to nil) :thread (append thread nil)
                    :body body))))))))))

(defun free-agent-bus--channel-key (from to)
  "Return the channel key for a post FROM directed at TO.
The commons (any TO of \"all\") is the single channel \"all\"; otherwise
the channel is the sorted participant set FROM∪TO, so both directions of
a pairwise conversation share one foldable section."
  (if (seq-find (lambda (c) (equal c "all")) to)
      "all"
    (mapconcat #'identity
               (sort (seq-uniq (cons from (copy-sequence to))) #'string<)
               ",")))

(defun free-agent-bus-log--render (raw)
  "Render RAW log text as org-shaped deck text.
Returns (TEXT . NEWEST-CHANNEL): TEXT to insert, NEWEST-CHANNEL the
1-based index of the channel section holding the newest post (nil when
no posts render).  Channels are participant sets (see
`free-agent-bus--channel-key'), emitted in first-seen order with one
colored circle per seat.  A post renders as a card fragment
`from ⟜ body' with a trailing id@ts tag; a reply nests one headline
level under its parent when the parent rendered in the same section,
else it carries a ↩ mark at section level."
  (let ((posts nil)
        (errors nil))
    (dolist (line (free-agent-bus-log--complete-lines raw))
      (let ((p (free-agent-bus-log--parse-post line)))
        (cond ((eq p :unparseable)
               (push (format "⟝ UNPARSEABLE (raw line kept in %s): %.160s"
                             (free-agent-bus--log-file) line)
                     errors))
              (p (push p posts)))))
    (setq posts (nreverse posts)
          errors (nreverse errors))
    (let ((channels nil)                       ; list of (key . posts), newest cell first
          (order nil)                          ; keys in first-seen order
          (id-loc (make-hash-table :test 'equal))) ; id -> (channel-key . depth)
      (dolist (p posts)
        (let* ((key (free-agent-bus--channel-key (plist-get p :from) (plist-get p :to)))
               (cell (assoc key channels)))
          (unless cell
            (setq cell (cons key nil))
            (push cell channels)
            (push key order))
          (push p (cdr cell))
          (puthash (plist-get p :id) (cons key 1) id-loc)))
      (let* ((out errors)                      ; parse-error rows lead, top level
             (order-r (nreverse order))
             (newest-key (and posts
                              (free-agent-bus--channel-key
                               (plist-get (car (last posts)) :from)
                               (plist-get (car (last posts)) :to))))
             (newest-idx (and newest-key
                              (seq-position order-r newest-key #'equal)))
             (newest-channel (and newest-idx (1+ newest-idx)))
             (channel-index 0))
        (dolist (cell channels)
          (setf (cdr cell) (nreverse (cdr cell))))
        (dolist (key order-r)
          (setq channel-index (1+ channel-index))
          (let* ((cell (assoc key channels))
                 (members (if (equal key "all")
                              '("all")
                            (split-string key "," t)))
                 (circles (mapconcat #'free-agent-bus--circle members " "))
                 (header (format "* %s %s"
                                 circles
                                 (if (equal key "all") "commons" key))))
            (push header out)
            (dolist (p (cdr cell))
              (let* ((id (plist-get p :id))
                     (parent (car (last (plist-get p :thread))))
                     (parent-loc (and parent (gethash parent id-loc)))
                     (same-section (and parent-loc (equal (car parent-loc) key)))
                     (depth (if same-section (1+ (cdr parent-loc)) 1))
                     (orphan (and parent (not same-section)))
                     (level (1+ (min depth 3)))
                     (meta (if free-agent-bus-log-show-timestamps
                               (propertize (format "  ⟪%s@%s⟫" id (plist-get p :ts))
                                           'face 'free-agent-bus-meta-face)
                             ""))
                     (head (format "%s %s ⟜ %s%s%s"
                                   (make-string level ?*)
                                   (plist-get p :from)
                                   (if orphan "↩ " "")
                                   (plist-get p :body)
                                   meta)))
                (puthash id (cons key depth) id-loc)
                (push head out)))))
        (cons (mapconcat #'identity (nreverse out) "\n")
              newest-channel)))))

(defun free-agent-bus-log--apply-default-folds (newest-channel)
  "Fold every channel section except NEWEST-CHANNEL (1-based, nil = all).
Fresh renders are fully unfolded; this applies the deck's default view:
current channel open, the rest one line each.  TAB re-folds manually."
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward "^\\* " nil t)
        (setq n (1+ n))
        (goto-char (match-beginning 0))
        (unless (and newest-channel (= n newest-channel))
          (condition-case nil
              (org-cycle nil)
            (error nil)))
        (end-of-line)))))

(defun free-agent-bus-log-refresh (&optional force)
  "Refresh the log buffer from the file, preserving scroll position.
When FORCE is non-nil, re-render even if the file content appears
unchanged. Keeps the tail in view for any window that was already at
the end of the log.  Parse-error count-check: when the unparseable-line
count increases versus the previous render, the increase is announced
in the echo area (*Messages*).  After a fresh render, applies the
default view: every channel section folded except the newest one."
  (when-let ((buf (get-buffer free-agent-bus-log-buffer-name)))
    (with-current-buffer buf
      (let ((previous-errors free-agent-bus-log--parse-errors))
        (setq free-agent-bus-log--parse-errors 0)
        (let* ((inhibit-read-only t)
               (rendered (free-agent-bus-log--render (free-agent-bus-log--read-file)))
               (contents (car rendered))
               (newest-channel (cdr rendered)))
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
              (free-agent-bus-log--apply-default-folds newest-channel)
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
                                  (list free-agent-bus-name "all"))
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
