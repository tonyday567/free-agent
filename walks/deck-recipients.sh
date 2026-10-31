#!/usr/bin/env bash
# deck-recipients — the leader audience command works outside composition mode.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "the ya leader entry invoked from a random buffer"
new_root recipients
out=$(FREE_AGENT_WALK_ROOT="$ROOT" "$EMACS" -Q -batch \
  --eval '(defvar doom-leader-map (make-sparse-keymap))' \
  -l "$DECK" \
  --eval '(with-temp-buffer
            (fundamental-mode)
            (setq free-agent-bus-post-to (list "old")
                  free-agent-bus-post-public nil)
            (let ((cmd (lookup-key doom-leader-map (kbd "ya"))))
              (when (commandp cmd)
                (funcall cmd "yin,shen"))
              (princ (format "command=%S\nmode=%S\nrecipients=%s\npublic=%S"
                             cmd major-mode
                             (mapconcat (function identity) free-agent-bus-post-to ",")
                             free-agent-bus-post-public))))')
assert_eq "SPC y a resolves and sets the audience outside post mode" \
  $'command=free-agent-bus-set-recipients\nmode=fundamental-mode\nrecipients=yin,shen\npublic=nil' "$out"

finish
