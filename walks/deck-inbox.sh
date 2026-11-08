#!/usr/bin/env bash
# deck-inbox — fresh mail is visible without a cursor; m marks every printed post.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "two posts for the deck and one for somebody else, with no cursor"
new_root inbox
post alice runner "first"
post alice bob "not for the deck"
post alice runner "second"

step "fresh read creates no state; m consumes exactly the two printed posts"
out=$(deck_eval '(progn
  (free-agent-bus-inbox)
  (let ((fresh (walk-ids))
        (fresh-cursor (file-exists-p (free-agent-bus-inbox--cursor-path)))
        (key (lookup-key free-agent-bus-inbox-mode-map (kbd "m"))))
    (free-agent-bus-inbox-mark-read)
    (let ((marked (walk-ids))
          (cursor (with-temp-buffer
                    (insert-file-contents (free-agent-bus-inbox--cursor-path))
                    (string-trim (buffer-string))))
          (posted (free-agent-bus--cli-stdin
                   "third" "say" "--from" "alice" "--to" "runner")))
      (free-agent-bus-inbox-refresh)
      (princ (format "fresh=%s\nfresh-cursor=%S\nm-key=%S\nmarked=%s\ncursor=%s\npost-status=%S\nnew=%s"
                     fresh fresh-cursor key marked cursor (car posted)
                     (walk-ids))))))')
assert_eq "fresh visibility, all-printed mark-read, and later mail still unread" \
  $'fresh=0,2\nfresh-cursor=nil\nm-key=free-agent-bus-inbox-mark-read\nmarked=\ncursor=3\npost-status=0\nnew=3' "$out"

finish
