;;; test-free-agent-card.el --- Card rendering tests -*- lexical-binding: t; -*-
;; Run from this directory. `-L .` is the load path that lets the require
;; below find free-agent-card.el; without it batch Emacs dies on the require.
;;   emacs -Q --batch -L . -l test-free-agent-card.el -f ert-run-tests-batch-and-exit

(require 'cl-lib)
(require 'ert)
(require 'free-agent-card)

(defconst free-agent-card-test-day
  "/Users/tonyday567/coffee/loom/day.md")

(defun free-agent-card-test--text (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun free-agent-card-test--render-day ()
  (let ((text (free-agent-card-test--text free-agent-card-test-day)))
    (with-temp-buffer
      (insert text)
      (setq buffer-file-name free-agent-card-test-day)
      (free-agent-card-render))))

(defun free-agent-card-test--find-button (text)
  (goto-char (point-min))
  (search-forward text)
  (button-at (match-beginning 0)))

(defun free-agent-card-test--cleanup ()
  (dolist (name '("*card: day.md*" "*bus conversation*"))
    (when (get-buffer name)
      (kill-buffer name))))

(ert-deftest free-agent-card-day-preserves-text-and-links-real-references ()
  (unwind-protect
      (let ((original (free-agent-card-test--text free-agent-card-test-day))
            (view (free-agent-card-test--render-day))
            opened-post opened-seat opened-involvement)
        (with-current-buffer view
          (should (equal (buffer-string) original))
          (let ((post (free-agent-card-test--find-button "[473]"))
                (seat (free-agent-card-test--find-button "[📬 bui]")))
            (should post)
            (should seat)
            (cl-letf (((symbol-function 'bus-conversation-open-post)
                       (lambda (id) (setq opened-post id)))
                      ((symbol-function 'bus-conversation-open-seat)
                       (lambda (name) (setq opened-seat name)))
                      ((symbol-function 'bus-conversation-open-seat-involvement)
                       (lambda (name) (setq opened-involvement name))))
              (button-activate post)
              (button-activate seat))
            (should (equal opened-post 473))
            ;; The 📬 mark asks for the direct runner ↔ seat conversation,
            ;; not the wider involvement view.
            (should (equal opened-seat "bui"))
            (should-not opened-involvement))
          (goto-char (point-min))
          (search-forward "[next]")
          (should-not (button-at (match-beginning 0)))
          (goto-char (point-min))
          (search-forward "[444–447]")
          (should-not (button-at (match-beginning 0)))))
    (free-agent-card-test--cleanup)))

(ert-deftest free-agent-card-day-opens-real-post-and-seat-conversations ()
  (unwind-protect
      (let ((view (free-agent-card-test--render-day)))
        (with-current-buffer view
          (button-activate (free-agent-card-test--find-button "[473]")))
        (with-current-buffer "*bus conversation*"
          (bus-conversation--stop-timer)
          (should (equal bus-conversation--focus 473))
          (should (cl-find 473 bus-conversation--posts :key #'bus-conversation--id))
          (should (equal bus-conversation--draft-thread 473))
          (should bus-conversation--draft-recipients)
          (goto-char bus-conversation--draft-start)
          (insert "local reply probe")
          (should (equal (buffer-substring-no-properties
                          bus-conversation--draft-start (point-max))
                         "local reply probe")))
        (with-current-buffer view
          (button-activate (free-agent-card-test--find-button "[📬 bui]")))
        (with-current-buffer "*bus conversation*"
          (bus-conversation--stop-timer)
          (should (equal bus-conversation--seat "bui"))
          ;; Post 473 is `from bui to runner`, so it belongs to the direct view.
          (should (cl-find 473 bus-conversation--posts :key #'bus-conversation--id))
          ;; `[📬 bui]' asks for the direct runner ↔ bui view, so the header,
          ;; the involvement flag and the draft recipients all say so.
          (should (string-prefix-p "runner ↔ bui · " (buffer-string)))
          (should-not bus-conversation--involvement)
          (should (equal bus-conversation--draft-recipients '("bui")))))
    (free-agent-card-test--cleanup)))

(ert-deftest free-agent-card-seat-involvement-includes-non-runner-threads ()
  (let ((posts '(((id . 1) (from . "runner") (to . ("bui")))
                 ((id . 2) (from . "bui") (to . ("case-officer")))
                 ((id . 3) (from . "case-officer") (to . ("bui" "runner")))
                 ((id . 4) (from . "duey") (to . ("runner"))))))
    (should (equal (mapcar (lambda (post) (alist-get 'id post))
                           (bus-conversation--seat-involvement posts "bui"))
                   '(1 2 3)))))

(ert-deftest free-agent-card-unknown-brackets-stay-inert ()
  (with-temp-buffer
    (insert "[next] [not-a-seat] [#999999999] [444–447]")
    (setq free-agent-card--directory "/Users/tonyday567/coffee/loom/")
    (free-agent-card--linkify
     (bus-conversation--read-posts bus-conversation-root))
    (goto-char (point-min))
    (should-not (button-at (point)))
    (should-not (next-button (point) t))))

(ert-deftest free-agent-card-markdown-file-link-resolves-beside-day-card ()
  (unwind-protect
      (let ((view (free-agent-card-test--render-day))
            opened)
        (with-current-buffer view
          (let ((button (free-agent-card-test--find-button
                         "[loop-synthesis](loop-synthesis.md)")))
            (should button)
            (cl-letf (((symbol-function 'find-file)
                       (lambda (path) (setq opened path))))
              (button-activate button)))
          (should (equal opened
                         "/Users/tonyday567/coffee/loom/loop-synthesis.md"))))
    (free-agent-card-test--cleanup)))

(defconst free-agent-card-test--shared-recipient-posts
  '(((id . 5) (from . "runner") (to . ("bui" "scully")) (body . "wake bui and scully"))
    ((id . 6) (from . "bui") (to . ("runner")) (body . "bui replies")))
  "Posts whose focus post addresses more than one seat, so the two views differ.")

(ert-deftest free-agent-card-seat-brackets-separate-direct-from-involvement ()
  "`[📬 seat]' opens the direct view; a bare seat keeps the involvement view."
  (with-temp-buffer
    (insert "[📬 bui] opens the direct view; [bui] keeps involvement.")
    (setq free-agent-card--directory temporary-file-directory)
    (free-agent-card--linkify free-agent-card-test--shared-recipient-posts)
    (let (direct involvement)
      (cl-letf (((symbol-function 'bus-conversation-open-seat)
                 (lambda (name) (setq direct name)))
                ((symbol-function 'bus-conversation-open-seat-involvement)
                 (lambda (name) (setq involvement name))))
        (button-activate (free-agent-card-test--find-button "[📬 bui]"))
        (should (equal direct "bui"))
        (should-not involvement)
        (button-activate (free-agent-card-test--find-button "[bui]"))
        (should (equal involvement "bui")))
      (should (equal (buffer-string)
                     "[📬 bui] opens the direct view; [bui] keeps involvement.")))))

(ert-deftest free-agent-card-direct-draft-addresses-only-the-seat ()
  "The direct view drafts to the seat; involvement and post views keep the focus post's recipients."
  (let ((posts free-agent-card-test--shared-recipient-posts)
        (root bus-conversation-root))
    (unwind-protect
        (progn
          (bus-conversation--open posts posts root 5 t "bui" nil)
          (with-current-buffer "*bus conversation*"
            (bus-conversation--stop-timer)
            (should (equal bus-conversation--draft-recipients '("bui")))
            (should (string-match-p "· to bui ---" (buffer-string))))
          (bus-conversation--open posts posts root 5 t "bui" t)
          (with-current-buffer "*bus conversation*"
            (bus-conversation--stop-timer)
            (should (equal bus-conversation--draft-recipients '("bui" "scully"))))
          (bus-conversation--open posts posts root 5 t nil nil)
          (with-current-buffer "*bus conversation*"
            (bus-conversation--stop-timer)
            (should (equal bus-conversation--draft-recipients '("bui" "scully")))))
      (bus-conversation--stop-timer)
      (free-agent-card-test--cleanup))))

(ert-deftest free-agent-card-direct-draft-survives-refresh ()
  "A refresh keeps the direct view's draft text and its seat-only recipients."
  (let ((posts free-agent-card-test--shared-recipient-posts)
        (root bus-conversation-root))
    (unwind-protect
        (progn
          (bus-conversation--open posts posts root 5 nil "bui" nil)
          (with-current-buffer "*bus conversation*"
            (bus-conversation--stop-timer)
            (should (equal bus-conversation--draft-recipients '("bui")))
            (goto-char bus-conversation--draft-start)
            (insert "local draft probe")
            (bus-conversation-refresh)
            (bus-conversation--stop-timer)
            (should (equal bus-conversation--draft-recipients '("bui")))
            (should (equal (buffer-substring-no-properties
                            bus-conversation--draft-start (point-max))
                           "local draft probe"))))
      (bus-conversation--stop-timer)
      (free-agent-card-test--cleanup))))

;; This witness runs the live bus root end to end.  It does not discriminate on
;; its own: the newest shape post already addressed shape, so the old draft rule
;; produced the same recipients.  The discriminating witnesses are
;; `free-agent-card-seat-brackets-separate-direct-from-involvement' and
;; `free-agent-card-direct-draft-addresses-only-the-seat'.
(ert-deftest free-agent-card-direct-seat-link-drafts-to-the-seat ()
  "Opening a seat directly drafts to that seat and says so in the header."
  (unwind-protect
      (progn
        (bus-conversation-open-seat "shape")
        (with-current-buffer "*bus conversation*"
          (bus-conversation--stop-timer)
          (should (equal bus-conversation--draft-recipients '("shape")))
          (should (string-match-p "runner ↔ shape" (buffer-string)))
          (should (string-match-p "· to shape ---" (buffer-string)))))
    (bus-conversation--stop-timer)
    (free-agent-card-test--cleanup)))

(ert-run-tests-batch-and-exit)
