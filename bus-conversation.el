;;; bus-conversation.el --- Live bus conversations -*- lexical-binding: t -*-

(require 'button)
(require 'cl-lib)
(require 'subr-x)

(defvar bus-conversation-root (expand-file-name "~/dev/bus"))
(defvar bus-conversation-coffee-root (expand-file-name "~/coffee"))
(defvar bus-conversation-refresh-interval 2)

(defvar-local bus-conversation--posts nil)
(defvar-local bus-conversation--all-posts nil)
(defvar-local bus-conversation--focus nil)
(defvar-local bus-conversation--seat nil)
(defvar-local bus-conversation--involvement nil)
(defvar-local bus-conversation--root nil)
(defvar-local bus-conversation--live nil)
(defvar-local bus-conversation--status nil)
(defvar-local bus-conversation--draft-start nil)
(defvar-local bus-conversation--draft-thread nil)
(defvar-local bus-conversation--draft-recipients nil)
(defvar-local bus-conversation--signature nil)
(defvar-local bus-conversation--timer nil)

(defvar bus-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'bus-conversation-refresh)
    (define-key map (kbd "C-c C-c") #'bus-conversation-submit-draft)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode bus-conversation-mode fundamental-mode "BusConversation"
  "Read and reply to bus conversations."
  (setq buffer-read-only nil)
  (add-hook 'kill-buffer-hook #'bus-conversation--stop-timer nil t))

(defun bus-conversation--id (post)
  (alist-get 'id post))

(defun bus-conversation--parse (text)
  (let (posts)
    (dolist (line (split-string text "\n" t))
      (when (string-match-p "^[ \t]*{" line)
        (push (json-parse-string
               (replace-regexp-in-string "\\s-+↩[0-9]+\\s-*\\'" "" line)
               :object-type 'alist :array-type 'list)
              posts)))
    (nreverse posts)))

(defun bus-conversation--log-file (root)
  (expand-file-name "log.jsonl" root))

(defun bus-conversation--read-posts (root)
  (with-temp-buffer
    (insert-file-contents (bus-conversation--log-file root))
    (bus-conversation--parse (buffer-string))))

(defun bus-conversation--signature (root)
  (let ((attributes (file-attributes (bus-conversation--log-file root))))
    (when attributes
      (list (file-attribute-size attributes)
            (file-attribute-modification-time attributes)))))

(defun bus-conversation--seat-status (seat root)
  (if (not root)
      "no current status"
    (with-temp-buffer
      (let ((exit (call-process "free-agent" nil t nil
                                "bus" "status" "-r" root "--live")))
        (if (not (equal exit 0))
            "status unavailable"
          (goto-char (point-min))
          (if (re-search-forward
               (concat "^\\S-+ " (regexp-quote seat) " —.*$") nil t)
              (match-string 0)
            "no current status"))))))

(defun bus-conversation--conversation (posts id)
  (let ((ids (list id)))
    (dotimes (_ (1+ (length posts)))
      (dolist (post posts)
        (let ((post-id (bus-conversation--id post))
              (parents (alist-get 'thread post)))
          (when (or (member post-id ids) (cl-intersection parents ids))
            (unless (member post-id ids) (push post-id ids))
            (dolist (parent parents)
              (unless (member parent ids) (push parent ids)))))))
    (cl-sort (cl-remove-if-not
              (lambda (post) (member (bus-conversation--id post) ids))
              (copy-sequence posts))
             (lambda (a b)
               (let ((at (alist-get 'ts a)) (bt (alist-get 'ts b)))
                 (if (equal at bt)
                     (< (bus-conversation--id a) (bus-conversation--id b))
                   (string< at bt)))))))

(defun bus-conversation--seat-posts (posts seat)
  (cl-remove-if-not
   (lambda (post)
     (or (and (equal (alist-get 'from post) "runner")
              (member seat (alist-get 'to post)))
         (and (equal (alist-get 'from post) seat)
              (member "runner" (alist-get 'to post)))))
   posts))

(defun bus-conversation--seat-involvement (posts seat)
  "Return every post from or addressed to SEAT."
  (cl-remove-if-not
   (lambda (post)
     (or (equal (alist-get 'from post) seat)
         (member seat (alist-get 'to post))))
   posts))

(defun bus-conversation--recipients (post &optional seat)
  (or (delete-dups
       (delete "runner"
               (copy-sequence
                (cons (alist-get 'from post) (alist-get 'to post)))))
      (and seat (list seat))))

(defun bus-conversation--card-path (card)
  (let ((direct (expand-file-name card bus-conversation-coffee-root)))
    (if (file-name-directory card)
        direct
      (or (cl-find-if
           #'file-exists-p
           (mapcar (lambda (directory)
                     (expand-file-name
                      card (expand-file-name directory bus-conversation-coffee-root)))
                   '("" "loom" "buff" "stuff")))
          direct))))

(defun bus-conversation-open-card (card &optional anchor)
  "Open CARD under the coffee root, optionally at ANCHOR's section."
  (find-file (bus-conversation--card-path card))
  (when anchor
    (let ((case-fold-search t)
          (heading (replace-regexp-in-string "-" " " anchor)))
      (goto-char (point-min))
      (unless (re-search-forward
               (concat "^#+\\s-+" (regexp-quote heading) "\\s-*$") nil t)
        (user-error "Section #%s not found in %s" anchor card))
      (goto-char (match-beginning 0))
      (recenter))))

(defun bus-conversation--linkify (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
            "\\[\\([^]\n]+\\)](\\([^)#\n]+\\.md\\)#\\([^)\n]+\\))" end t)
      (let ((card (match-string 2))
            (anchor (match-string 3))
            (button-beg (match-beginning 0))
            (button-end (match-end 0)))
        (make-button button-beg button-end
                     'action (lambda (_) (bus-conversation-open-card card anchor))
                     'follow-link t
                     'help-echo (format "Open %s#%s" card anchor))))
    (goto-char beg)
    (while (re-search-forward "\\[#\\([0-9]+\\)\\]" end t)
      (let ((id (string-to-number (match-string 1)))
            (button-beg (match-beginning 0))
            (button-end (match-end 0)))
        (make-button button-beg button-end
                     'action (lambda (_) (bus-conversation-open-post id))
                     'follow-link t
                     'help-echo (format "Open post #%d" id))))
    (goto-char beg)
    (while (re-search-forward "\\[\\([^]\n]+\\.md\\)\\]" end t)
      (let ((card (match-string 1))
            (button-beg (match-beginning 0))
            (button-end (match-end 0)))
        (make-button button-beg button-end
                     'action (lambda (_) (bus-conversation-open-card card))
                     'follow-link t
                     'help-echo (format "Open %s" card))))
    (goto-char beg)
    (while (re-search-forward "\\[\\([a-z][a-z0-9-]*\\)\\]" end t)
      (let ((seat (match-string 1))
            (button-beg (match-beginning 0))
            (button-end (match-end 0)))
        (unless (equal seat "runner")
          (make-button button-beg button-end
                       'action (lambda (_) (bus-conversation-open-seat seat))
                       'follow-link t
                       'help-echo (format "Open runner ↔ %s" seat)))))))

(defun bus-conversation--insert-post (post)
  (let ((beg (point)))
    (when (equal (alist-get 'from post) "runner")
      (insert "⬡ "))
    (let ((body-beg (point)))
      (insert (alist-get 'body post) "\n\n")
      (bus-conversation--linkify body-beg (- (point) 2)))
    (cons (bus-conversation--id post) beg)))

(defun bus-conversation--capture-state ()
  (list :draft (if (markerp bus-conversation--draft-start)
                   (buffer-substring-no-properties
                    bus-conversation--draft-start (point-max))
                 "")
        :draft-offset (when (and (markerp bus-conversation--draft-start)
                                 (>= (point) bus-conversation--draft-start))
                        (- (point) bus-conversation--draft-start))
        :point (point)
        :window-start (when (eq (window-buffer) (current-buffer))
                        (window-start))))

(defun bus-conversation--render ()
  (let ((state (bus-conversation--capture-state))
        (inhibit-read-only t)
        positions)
    (with-silent-modifications
      (erase-buffer)
      (when bus-conversation--seat
        (insert (format (if bus-conversation--involvement
                            "%s · %s · thread involvement\n\n"
                          "runner ↔ %s · %s\n\n")
                        bus-conversation--seat bus-conversation--status)))
      (when bus-conversation--focus
        (insert-text-button
         (format "[#%d]" bus-conversation--focus)
         'action (lambda (_) (bus-conversation-open-post bus-conversation--focus))
         'follow-link t
         'help-echo (format "Open post #%d" bus-conversation--focus))
        (insert "\n\n"))
      (if (not bus-conversation--posts)
          (insert "(no conversation posts)\n\n")
        (dolist (post bus-conversation--posts)
          (push (bus-conversation--insert-post post) positions)))
      (insert (format "--- draft · thread %s · to %s ---\n"
                      (if bus-conversation--draft-thread
                          (format "#%d" bus-conversation--draft-thread)
                        "none")
                      (if bus-conversation--draft-recipients
                          (string-join bus-conversation--draft-recipients ", ")
                        "none")))
      (setq bus-conversation--draft-start (point-marker))
      (insert (plist-get state :draft))
      (add-text-properties (point-min) bus-conversation--draft-start
                           '(read-only t))
      (add-text-properties (1- bus-conversation--draft-start)
                           bus-conversation--draft-start
                           '(rear-nonsticky t))
      (remove-list-of-text-properties bus-conversation--draft-start (point-max)
                                      '(read-only)))
    (setq positions (nreverse positions))
    (if (plist-get state :draft-offset)
        (goto-char (+ bus-conversation--draft-start
                      (plist-get state :draft-offset)))
      (goto-char (min (plist-get state :point) (point-max))))
    (when (plist-get state :window-start)
      (set-window-start (selected-window) (plist-get state :window-start) t))
    positions))

(defun bus-conversation--stop-timer ()
  (when (timerp bus-conversation--timer)
    (cancel-timer bus-conversation--timer))
  (setq bus-conversation--timer nil))

(defun bus-conversation--maybe-update (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when bus-conversation--live
        (let ((signature (bus-conversation--signature bus-conversation--root)))
          (unless (equal signature bus-conversation--signature)
            (setq bus-conversation--signature signature)
            (bus-conversation-refresh)))))))

(defun bus-conversation--start-timer ()
  (bus-conversation--stop-timer)
  (when (and bus-conversation--live bus-conversation-refresh-interval)
    (setq bus-conversation--timer
          (run-at-time bus-conversation-refresh-interval
                       bus-conversation-refresh-interval
                       #'bus-conversation--maybe-update (current-buffer)))))

(defun bus-conversation--open (posts all-posts root focus &optional live seat involvement)
  (unless posts
    (unless seat (user-error "No bus JSONL posts found")))
  (let ((buffer (get-buffer-create "*bus conversation*"))
        (focus-post (cl-find focus posts :key #'bus-conversation--id))
        positions)
    (with-current-buffer buffer
      (bus-conversation-mode)
      (bus-conversation--stop-timer)
      (setq bus-conversation--posts posts
            bus-conversation--all-posts all-posts
            bus-conversation--root root
            bus-conversation--focus focus
            bus-conversation--seat seat
            bus-conversation--involvement involvement
            bus-conversation--live live
            bus-conversation--status (and seat (bus-conversation--seat-status seat root))
            bus-conversation--draft-start nil
            bus-conversation--draft-thread focus
            bus-conversation--draft-recipients
            ;; The direct seat view addresses the seat, whatever the focus
            ;; post happened to carry; every other caller keeps the focus
            ;; post's recipients.
            (if (and seat (not involvement))
                (list seat)
              (if focus-post
                  (bus-conversation--recipients focus-post seat)
                (and seat (list seat))))
            bus-conversation--signature (and live (bus-conversation--signature root)))
      (setq positions (bus-conversation--render))
      (bus-conversation--start-timer))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (let ((position (cdr (assq focus positions))))
        (when position
          (goto-char position)
          (recenter))))
    buffer))

(defun bus-conversation-refresh ()
  "Reload the current conversation while preserving its draft."
  (interactive)
  (unless bus-conversation--root
    (user-error "This fixture view has no live bus root"))
  (setq bus-conversation--all-posts
        (bus-conversation--read-posts bus-conversation--root))
  (setq bus-conversation--posts
        (if bus-conversation--seat
            (if bus-conversation--involvement
                (bus-conversation--seat-involvement bus-conversation--all-posts
                                                   bus-conversation--seat)
              (bus-conversation--seat-posts bus-conversation--all-posts
                                            bus-conversation--seat))
          (bus-conversation--conversation bus-conversation--all-posts
                                          bus-conversation--focus)))
  (setq bus-conversation--signature
        (bus-conversation--signature bus-conversation--root))
  (bus-conversation--render))

(defun bus-conversation-open-post (id)
  "Open post ID centered in its conversation."
  (interactive (list (read-number "Post id: ")))
  (let* ((in-view (derived-mode-p 'bus-conversation-mode))
         (all-posts (if in-view bus-conversation--all-posts nil))
         (root (if in-view bus-conversation--root nil)))
    (unless (cl-find id all-posts :key #'bus-conversation--id)
      (setq root bus-conversation-root
            all-posts (bus-conversation--read-posts root)))
    (unless (cl-find id all-posts :key #'bus-conversation--id)
      (user-error "Post #%d not found" id))
    (bus-conversation--open
     (bus-conversation--conversation all-posts id)
     all-posts root id (not (null root)))))

(defun bus-conversation-open-seat (seat)
  "Open the runner and SEAT conversation with current status."
  (interactive (list (read-string "Seat: ")))
  (let* ((in-view (derived-mode-p 'bus-conversation-mode))
         (all-posts (if in-view bus-conversation--all-posts
                      (bus-conversation--read-posts bus-conversation-root)))
         (root (if in-view bus-conversation--root bus-conversation-root))
         (posts (bus-conversation--seat-posts all-posts seat))
         (latest (car (last posts))))
    (bus-conversation--open posts all-posts root
                            (and latest (bus-conversation--id latest))
                            (not (null root)) seat)))

(defun bus-conversation-open-seat-involvement (seat)
  "Open SEAT's current status and every thread it participates in."
  (interactive (list (read-string "Seat: ")))
  (let* ((in-view (derived-mode-p 'bus-conversation-mode))
         (all-posts (if in-view bus-conversation--all-posts
                      (bus-conversation--read-posts bus-conversation-root)))
         (root (if in-view bus-conversation--root bus-conversation-root))
         (posts (bus-conversation--seat-involvement all-posts seat))
         (latest (car (last posts))))
    (bus-conversation--open posts all-posts root
                            (and latest (bus-conversation--id latest))
                            (not (null root)) seat t)))

(defun bus-conversation-open-log ()
  "Open the latest live bus conversation."
  (interactive)
  (let* ((all-posts (bus-conversation--read-posts bus-conversation-root))
         (latest (car (last all-posts))))
    (unless latest (user-error "No bus JSONL posts found"))
    (bus-conversation--open
     (bus-conversation--conversation all-posts (bus-conversation--id latest))
     all-posts bus-conversation-root (bus-conversation--id latest) t)))

(defun bus-conversation-submit-draft ()
  "Submit the draft as an ordinary runner bus post."
  (interactive)
  (unless (and bus-conversation--root bus-conversation--draft-recipients)
    (user-error "Draft has no bus root or recipients"))
  (let ((body (string-trim
               (buffer-substring-no-properties
                bus-conversation--draft-start (point-max)))))
    (when (string-empty-p body)
      (user-error "Draft is empty"))
    (let ((arguments
           (append
            (list "bus" "say" "-r" bus-conversation--root "--from" "runner")
            (when bus-conversation--draft-thread
              (list "--thread" (number-to-string bus-conversation--draft-thread)))
            (cl-mapcan (lambda (recipient)
                         (list "--to" recipient))
                       bus-conversation--draft-recipients))))
      (with-temp-buffer
        (insert body)
        (let ((exit (apply #'call-process-region
                           (point-min) (point-max) "free-agent"
                           nil "*bus conversation submit*" nil arguments)))
          (unless (equal exit 0)
            (user-error "Bus post failed; draft retained"))))
      (let ((inhibit-read-only t))
        (with-silent-modifications
          (delete-region bus-conversation--draft-start (point-max))))
      (bus-conversation-refresh))))

(defun bus-jsonl-conversation-view (beg end)
  "Show bus JSONL between BEG and END as a linked conversation."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((posts (bus-conversation--parse
                 (buffer-substring-no-properties beg end)))
         (first (car posts))
         (live (and (not (use-region-p))
                    (string-prefix-p "*free-agent-bus-" (buffer-name))
                    (file-exists-p (bus-conversation--log-file bus-conversation-root))))
         (all-posts (if live (bus-conversation--read-posts bus-conversation-root)
                      posts)))
    (unless first (user-error "No bus JSONL posts found"))
    (bus-conversation--open posts all-posts
                            (and live bus-conversation-root)
                            (bus-conversation--id first) live)))

(provide 'bus-conversation)
;;; bus-conversation.el ends here
