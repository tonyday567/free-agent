#!/usr/bin/env bash
# deck-depth — TAB's command cycles log, inbox and thread views through 0/1/2.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "a two-post thread with mail for the deck"
new_root depth
post alice runner "root"
printf 'reply' | "$FA" bus post -r "$ROOT" --from alice --to runner --thread 0 >/dev/null

step "each surface cycles 1 → 2 → 0 → 1 and binds both Tab event names"
out=$(deck_eval '(progn
  (defun walk-depths (variable)
    (let ((depths (list (symbol-value variable))))
      (dotimes (_ 3)
        (free-agent-bus-cycle-depth)
        (setq depths (append depths (list (symbol-value variable)))))
      depths))
  (defun walk-depth-key-p (map key)
    (eq (lookup-key map key) (function free-agent-bus-cycle-depth)))
  (free-agent-bus-log)
  (let ((log-depths (walk-depths (quote free-agent-bus-log-depth))))
    (free-agent-bus-inbox)
    (let ((inbox-depths (walk-depths (quote free-agent-bus-inbox-depth))))
      (free-agent-bus-thread 0)
      (let ((thread-depths (walk-depths (quote free-agent-bus-view-depth)))
            (keys (append
                   (mapcar (lambda (key)
                             (walk-depth-key-p free-agent-bus-log-mode-map key))
                           (list (kbd "TAB") (kbd "<tab>")))
                   (mapcar (lambda (key)
                             (walk-depth-key-p free-agent-bus-inbox-mode-map key))
                           (list (kbd "TAB") (kbd "<tab>")))
                   (mapcar (lambda (key)
                             (walk-depth-key-p free-agent-bus-view-mode-map key))
                           (list (kbd "TAB") (kbd "<tab>"))))))
        (princ (format "log=%S\ninbox=%S\nthread=%S\nkeys=%S"
                       log-depths inbox-depths thread-depths keys))))))')
assert_eq "log, inbox and thread depth cycles plus their TAB bindings" \
  $'log=(1 2 0 1)\ninbox=(1 2 0 1)\nthread=(1 2 0 1)\nkeys=(t t t t t t)' "$out"

finish
