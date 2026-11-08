;;; free-agent-card.el --- Render cards with optional bus links -*- lexical-binding: t; -*-

;; A card is plain Markdown.  This rendered view preserves its text exactly and
;; adds buttons only where the bus or filesystem supplies a real target.
;; Conversation and reply live in bus-conversation.el, not in this mode.

;;; Code:

(require 'button)
(require 'bus-conversation)
(require 'subr-x)

(defgroup free-agent-card nil
  "Read coffee cards with optional bus and file links."
  :group 'applications)

(defvar-local free-agent-card--source nil)
(defvar-local free-agent-card--directory nil)

(defun free-agent-card--participants (posts)
  "Return the seat names present in POSTS, excluding the runner."
  (let (seats)
    (dolist (post posts)
      (push (alist-get 'from post) seats)
      (dolist (recipient (alist-get 'to post))
        (push recipient seats)))
    (delete "runner" (delete-dups (delq nil seats)))))

(defun free-agent-card--open-file (directory target &optional anchor)
  "Open TARGET relative to DIRECTORY, optionally at ANCHOR's heading."
  (find-file (expand-file-name target directory))
  (when anchor
    (let ((case-fold-search t)
          (heading (replace-regexp-in-string "-" " " anchor)))
      (goto-char (point-min))
      (unless (re-search-forward
               (concat "^#+\\s-+" (regexp-quote heading) "\\s-*$") nil t)
        (user-error "Section #%s not found in %s" anchor target))
      (goto-char (match-beginning 0))
      (recenter))))

(defun free-agent-card--file-target (target)
  "Return TARGET's path and anchor when it names an existing local file."
  (unless (string-match-p "\\`[a-z]+://" target)
    (let* ((parts (split-string target "#" t))
           (path (car parts))
           (anchor (cadr parts))
           (absolute (expand-file-name path free-agent-card--directory)))
      (when (file-exists-p absolute)
        (list path anchor)))))

(defun free-agent-card--button (beg end action help)
  "Make BEG through END a button calling ACTION with HELP text."
  (unless (button-at beg)
    (make-button beg end
                 'action action
                 'follow-link t
                 'help-echo help)))

(defun free-agent-card--linkify (posts)
  "Add resolved file, post and seat links to the current card.
POSTS supplies the valid post ids and participant names.  Ranges, [next],
unknown brackets and unresolved paths remain unannotated text."
  (let ((ids (make-hash-table :test #'eql))
        (seats (make-hash-table :test #'equal)))
    (dolist (post posts)
      (puthash (alist-get 'id post) t ids))
    (dolist (seat (free-agent-card--participants posts))
      (puthash seat t seats))
    (save-excursion
      ;; Ordinary Markdown file links resolve relative to the source card.
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^]\n]+\\)](\\([^)\n]+\\))" nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0))
              (target (match-string 2)))
          (when-let ((resolved (free-agent-card--file-target target)))
            (let ((path (car resolved))
                  (anchor (cadr resolved))
                  (directory free-agent-card--directory))
              (free-agent-card--button
               beg end
               (lambda (_) (free-agent-card--open-file directory path anchor))
               (format "Open %s" target))))))
      ;; Both the day card's [473] and the prototype's [#473] use one resolver.
      (goto-char (point-min))
      (while (re-search-forward "\\[#?\\([0-9]+\\)]" nil t)
        (let ((id (string-to-number (match-string 1)))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (when (gethash id ids)
            (free-agent-card--button
             beg end
             (lambda (_) (bus-conversation-open-post id))
             (format "Open post #%d in its conversation" id)))))
      ;; A seat is known from the log, not guessed from lowercase prose.  The
      ;; 📬 mark asks for the direct runner ↔ seat view; a bare name keeps the
      ;; wider involvement view.
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(📬 \\)?\\([a-z][a-z0-9-]*\\)]" nil t)
        (let ((direct (match-string 1))
              (seat (match-string 2))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (when (and (gethash seat seats)
                     (not (equal seat "next")))
            (free-agent-card--button
             beg end
             (if direct
                 (lambda (_) (bus-conversation-open-seat seat))
               (lambda (_) (bus-conversation-open-seat-involvement seat)))
             (if direct
                 (format "Open the runner ↔ %s conversation" seat)
               (format "Open %s status and thread involvement" seat)))))))))

(defun free-agent-card--render-into (buffer source directory text)
  "Render SOURCE's TEXT into BUFFER using DIRECTORY for file links."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (free-agent-card-mode)
      (setq free-agent-card--source source
            free-agent-card--directory directory)
      (free-agent-card--linkify
       (bus-conversation--read-posts bus-conversation-root))
      (goto-char (point-min))
      (set-buffer-modified-p nil)))
  buffer)

;;;###autoload
(defun free-agent-card-render ()
  "Render the current file card with resolved bus and file links.
The source file is not modified.  Follow a post or seat link to read and
reply in its own conversation view."
  (interactive)
  (unless buffer-file-name
    (user-error "The current buffer is not visiting a card file"))
  (let* ((source buffer-file-name)
         (directory (file-name-directory source))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (buffer (get-buffer-create
                  (format "*card: %s*" (file-name-nondirectory source)))))
    (free-agent-card--render-into buffer source directory text)
    (pop-to-buffer buffer)))

(defun free-agent-card-refresh ()
  "Re-read this rendered card's source, preserving point where possible."
  (interactive)
  (unless free-agent-card--source
    (user-error "This is not a rendered card"))
  (let ((position (point))
        (text (with-temp-buffer
                (insert-file-contents free-agent-card--source)
                (buffer-string)))
        (source free-agent-card--source)
        (directory free-agent-card--directory))
    (free-agent-card--render-into (current-buffer) source directory text)
    (goto-char (min position (point-max)))))

(define-derived-mode free-agent-card-mode special-mode "Card"
  "Read a card with resolved file, post and seat links.
\\{free-agent-card-mode-map}"
  (setq truncate-lines nil)
  (setq-local revert-buffer-function
              (lambda (&rest _) (free-agent-card-refresh))))

(define-key free-agent-card-mode-map (kbd "g") #'free-agent-card-refresh)

(provide 'free-agent-card)
;;; free-agent-card.el ends here
