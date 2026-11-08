#!/usr/bin/env bash
# deck-inbox-to-here — M clears the old flood; mail after the point stays unread.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "four posts to the deck, no cursor"
new_root inbox
post alice runner "one"
post alice runner "two"
post alice runner "three"
post alice runner "four"

step "M on the second post clears through it and keeps the rest unread"
out=$(deck_eval '(progn
  (free-agent-bus-inbox)
  (let ((m-key (lookup-key free-agent-bus-inbox-mode-map (kbd "M"))))
    (goto-char (point-min))
    (search-forward "two")
    (free-agent-bus-inbox-mark-read-to-here)
    (let ((cursor (with-temp-buffer
                    (insert-file-contents (free-agent-bus-inbox--cursor-path))
                    (string-trim (buffer-string)))))
      (princ (format "m-key=%S\ncursor=%s\nremaining=%s" m-key cursor (walk-ids))))))')
assert_eq "mark read to here advances the cursor past the point and stops" \
  $'m-key=free-agent-bus-inbox-mark-read-to-here\ncursor=2\nremaining=2,3' "$out"

finish
