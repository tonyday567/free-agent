;;; test-free-agent-card.el --- Card rendering tests -*- lexical-binding: t; -*-

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
            opened-post opened-seat)
        (with-current-buffer view
          (should (equal (buffer-string) original))
          (let ((post (free-agent-card-test--find-button "[473]"))
                (seat (free-agent-card-test--find-button "[📬 bui]")))
            (should post)
            (should seat)
            (cl-letf (((symbol-function 'bus-conversation-open-post)
                       (lambda (id) (setq opened-post id)))
                      ((symbol-function 'bus-conversation-open-seat-involvement)
                       (lambda (name) (setq opened-seat name))))
              (button-activate post)
              (button-activate seat))
            (should (equal opened-post 473))
            (should (equal opened-seat "bui")))
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
          (should (cl-find 473 bus-conversation--posts :key #'bus-conversation--id))
          (should (string-prefix-p "bui · " (buffer-string)))
          (should (string-match-p " · thread involvement\n\n" (buffer-string)))
          (should bus-conversation--involvement)
          (should (member "bui" bus-conversation--draft-recipients))))
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

(ert-run-tests-batch-and-exit)
