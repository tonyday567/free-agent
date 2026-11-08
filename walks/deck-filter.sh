#!/usr/bin/env bash
# deck-filter — an audience filter keeps public posts and matching recipients.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "one public post and two addressed posts"
new_root filter
post alice - "public"
post alice yin "for yin"
post alice bob "for bob"

step "filtering by either name never hides the public post"
out=$(deck_eval '(progn
  (free-agent-bus-log)
  (let (rows)
    (free-agent-bus-log-set-filter (list "yin"))
    (push (concat "yin=" (walk-ids)) rows)
    (free-agent-bus-log-set-filter (list "bob"))
    (push (concat "bob=" (walk-ids)) rows)
    (free-agent-bus-log-set-filter nil)
    (push (concat "all=" (walk-ids)) rows)
    (princ (mapconcat (function identity) (reverse rows) "\n"))))')
assert_eq "matching, public and unfiltered ids" \
  $'yin=0,1\nbob=0,2\nall=0,1,2' "$out"

finish
