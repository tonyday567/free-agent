;;; card-composer-views.el --- bus Org views: channel, post, agent -*- lexical-binding: t; -*-

;; Complementary minimum to free1's card-composer.el, which retains the
;; open-thread/send assignment.  This file adds three commands, each opening
;; a temporary editable org-mode buffer rendered through free1's bus-side
;; path (`free-agent bus org'):
;;
;;   `card-composer-views-open-channel' — every thread whose root post
;;     carries the named channel.
;;   `card-composer-views-open-post'    — the thread containing one post id.
;;   `card-composer-views-open-agent'   — every thread involving the named
;;     agent, grouped as in the card-composer example:
;;       * kimi
;;       ** #105
;;       *** kimi
;;       hello
;;
;; No send wiring here: multi-thread views do not countermand free1's
;; open-thread [send] draft.  Buffers are plain `org-mode' and editable; the
;; bus stays append-only.
;;
;; Note: `bus org' is newer than the installed free-agent binary.  Point
;; `card-composer-executable' at a build that has it (see
;; ~/haskell/free-agent/dist-newstyle/.../free-agent) or `cabal build
;; exe:free-agent' in ~/haskell/free-agent first.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'card-composer)

(defun card-composer-views--org-thread (id)
  "Render ID's thread through `free-agent bus org' and return the Org text.
Signal a user-error carrying the renderer's 🔴 line on failure."
  (let ((result (apply #'card-composer--cli-stdin
                       ""
                       (list "org" "-r" (expand-file-name card-composer-root)
                             "--thread" (number-to-string id)))))
    (unless (zerop (car result))
      (user-error "%s" (cdr result)))
    (cdr result)))

(defun card-composer-views--read-posts ()
  "Parse the current bus log into plists (:id :ts :from :to :channel :thread)."
  (let* ((file (expand-file-name "log.jsonl" (expand-file-name card-composer-root)))
         (posts nil))
    (unless (file-readable-p file)
      (user-error "bus log not readable: %s" file))
    (let ((text (with-temp-buffer
                  (insert-file-contents-literally file)
                  (set-buffer-file-coding-system 'utf-8)
                  (decode-coding-region (point-min) (point-max) 'utf-8)
                  (buffer-string))))
      (dolist (line (split-string text "\n" t))
        (when-let ((obj (ignore-errors (json-read-from-string line))))
          (push (list :id (alist-get 'id obj)
                      :ts (alist-get 'ts obj)
                      :from (alist-get 'from obj)
                      :to (append (alist-get 'to obj) nil)
                      :channel (alist-get 'channel obj)
                      :thread (append (alist-get 'thread obj) nil))
                posts))))
    (nreverse posts)))

(defun card-composer-views--root-of (post idx)
  "Thread root of POST following ancestry through IDX (id → post).
Mirrors the CLI's ByThread slicing: an empty ancestry is its own root;
otherwise follow the earliest-stamped resolvable ancestor, falling back to
the smallest ancestry id when none resolve.  Cycles collapse to their
minimum id."
  (let ((p post)
        (seen nil)
        (done nil))
    (while (not done)
      (let ((pid (plist-get p :id))
            (thr (plist-get p :thread)))
        (cond
         ((memq pid seen)
          (setq done (apply #'min (cons pid seen))))
         ((null thr)
          (setq done pid))
         (t
          (let* ((resolved (seq-filter (lambda (tid) (gethash tid idx)) thr))
                 (next (if resolved
                           (car (seq-sort-by (lambda (tid)
                                               (plist-get (gethash tid idx) :ts))
                                             #'string< resolved))
                         (apply #'min thr))))
            (push pid seen)
            (setq p (gethash next idx)))))))
    done))

(defun card-composer-views--thread-roots (posts)
  "Group POSTS by thread root.  Return sorted (ROOT-ID . POSTS) pairs."
  (let ((idx (make-hash-table :test 'eql))
        (groups (make-hash-table :test 'eql)))
    (dolist (p posts)
      (puthash (plist-get p :id) p idx))
    (dolist (p posts)
      (let ((root (card-composer-views--root-of p idx)))
        (puthash root (cons p (gethash root groups)) groups)))
    (sort (cl-loop for root being the hash-keys of groups
                   collect (cons root (nreverse (gethash root groups))))
          (lambda (a b) (< (car a) (car b))))))

(defun card-composer-views--demote (rendered)
  "Demote a `bus org' thread render one Org level: `* [#N] rest' → `** #N rest',
`** sender' → `*** sender'.  Body lines pass through unchanged."
  (mapconcat
   (lambda (line)
     (cond
      ((string-match "^\\* \\[#\\([0-9]+\\)\\]\\(?: \\(.*\\)\\)?$" line)
       (format "** #%s%s" (match-string 1 line)
               (if (match-string 2 line) (concat " " (match-string 2 line)) "")))
      ((string-prefix-p "** " line)
       (concat "*" line))
      (t line)))
   (split-string rendered "\n")
   "\n"))

(defun card-composer-views--show (buffer-name text)
  "Display TEXT in BUFFER-NAME as a fresh temporary editable Org buffer."
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert text)
    (unless (string-suffix-p "\n" (buffer-string))
      (insert "\n"))
    (org-mode)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun card-composer-views-open-post (id)
  "Open the thread containing bus post ID in a temporary editable Org buffer.
Rendering is free1's `bus org' path; the heading carries the id asked for."
  (interactive "nPost id: ")
  (card-composer-views--show (format "*bus-post %d*" id)
                             (card-composer-views--org-thread id)))

;;;###autoload
(defun card-composer-views-open-channel (channel)
  "Open every bus thread whose root post carries CHANNEL.
Threads render through `bus org', demoted one level under `* CHANNEL'."
  (interactive "sChannel: ")
  (let* ((posts (card-composer-views--read-posts))
         (roots (cl-remove-if-not
                 (lambda (entry)
                   (equal (plist-get (cadr entry) :channel) channel))
                 (card-composer-views--thread-roots posts))))
    (unless roots
      (user-error "No threads with channel %s on %s"
                  channel (expand-file-name card-composer-root)))
    (card-composer-views--show
     (format "*bus-channel %s*" channel)
     (concat "* " channel "\n\n"
             (mapconcat
              (lambda (entry)
                (concat (card-composer-views--demote
                         (card-composer-views--org-thread (car entry)))
                        "\n"))
              roots "\n")))))

;;;###autoload
(defun card-composer-views-open-agent (agent)
  "Open every bus thread involving AGENT (sender or addressee of any post).
Threads render through `bus org', demoted one level, grouped as
`* AGENT' with one `** #ID' subtree per thread — the card-composer example."
  (interactive "sAgent: ")
  (let* ((posts (card-composer-views--read-posts))
         (roots (cl-remove-if-not
                 (lambda (entry)
                   (cl-some (lambda (p)
                              (or (equal (plist-get p :from) agent)
                                  (member agent (plist-get p :to))))
                            (cdr entry)))
                 (card-composer-views--thread-roots posts))))
    (unless roots
      (user-error "No threads involving %s on %s"
                  agent (expand-file-name card-composer-root)))
    (card-composer-views--show
     (format "*bus-agent %s*" agent)
     (concat "* " agent "\n\n"
             (mapconcat
              (lambda (entry)
                (concat (card-composer-views--demote
                         (card-composer-views--org-thread (car entry)))
                        "\n"))
              roots "\n")))))

(provide 'card-composer-views)
;;; card-composer-views.el ends here
