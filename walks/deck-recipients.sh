#!/usr/bin/env bash
# deck-recipients — audience changes outside composition mode refresh and govern the post.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "the ya leader entry invoked from a random buffer"
new_root recipients
out=$(FREE_AGENT_WALK_ROOT="$ROOT" FREE_AGENT_WALK_FA="$FA" "$EMACS" -Q -batch \
  --eval '(defvar doom-leader-map (make-sparse-keymap))' \
  -l "$DECK" \
  --eval '(progn
            (setq free-agent-bus-root (getenv "FREE_AGENT_WALK_ROOT")
                  free-agent-bus-executable (getenv "FREE_AGENT_WALK_FA")
                  free-agent-bus-name "runner"
                  free-agent-bus-auto-refresh-interval 3600.0
                  free-agent-bus-status-refresh-interval 3600.0)
            (let (cmd random-mode before-mode after-mode thread-mode)
              (free-agent-bus-post)
              (setq free-agent-bus-post-to (list "old")
                    free-agent-bus-post-public nil)
              (free-agent-bus--post-mode-refresh)
              (insert "audience from random buffer")
              (setq before-mode mode-name)
              (with-temp-buffer
                (fundamental-mode)
                (setq random-mode major-mode
                      cmd (lookup-key doom-leader-map (kbd "ya")))
                (when (commandp cmd)
                  (funcall cmd "yin,shen")))
              (setq after-mode
                    (with-current-buffer free-agent-bus-post-buffer-name mode-name))
              (with-current-buffer free-agent-bus-post-buffer-name
                (free-agent-bus-send (point-min) (point-max)))
              (with-temp-buffer
                (fundamental-mode)
                (free-agent-bus-set-thread 0))
              (setq thread-mode
                    (with-current-buffer free-agent-bus-post-buffer-name mode-name))
              (princ (format "command=%S\nmode=%S\nbefore=%s\nafter=%s\nthread=%s\nrecipients=%s\npublic=%S"
                             cmd random-mode before-mode after-mode thread-mode
                             (mapconcat (function identity) free-agent-bus-post-to ",")
                             free-agent-bus-post-public))))')
assert_eq "SPC y a resolves and sets the audience outside post mode" \
  $'command=free-agent-bus-set-recipients\nmode=fundamental-mode\nbefore=FAB-Post[old]\nafter=FAB-Post[yin,shen]\nthread=FAB-Post[yin,shen thread=0]\nrecipients=yin,shen\npublic=nil' "$out"
assert_eq "the composition send honours the externally set audience" \
  '["yin", "shen"]' "$(field 0 to)"
assert_eq "the sent body comes from the composition buffer" \
  '"audience from random buffer"' "$(field 0 body)"

finish
