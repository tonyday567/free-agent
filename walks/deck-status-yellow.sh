#!/usr/bin/env bash
# deck-status-yellow — the status page reads the loop's dots and f hides the parked.
#
# A busy seat reads 🟣, a fresh idle seat 🟢, an idle seat past the hour 🟡.
# The yellow filter drops the parked rows from the model without touching the
# busy or ready rows, and toggles back. Rows come from the same CLI the live
# page parses; nothing here opens a frame or touches a runner buffer.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "a root with a busy, a fresh and a parked seat"
new_root status
MYPID=$$
TWO_HOURS_AGO=$(date -u -v-2H +%Y-%m-%dT%H:%M:%S.000000Z)
NOW=$(date -u +%Y-%m-%dT%H:%M:%S.000000Z)
printf 'working %s %s' "$NOW" "$MYPID" >"$ROOT/.hb-busy"
printf 'idle %s %s' "$NOW" "$MYPID" >"$ROOT/.hb-fresh"
printf 'idle %s %s' "$TWO_HOURS_AGO" "$MYPID" >"$ROOT/.hb-parked"
say alice - "seed"

step "the model carries the dots, and f hides only the parked"
out=$(deck_eval '(progn
  (defun walk-status-dots ()
    (let (dots)
      (dolist (row (plist-get free-agent-bus-status--model :rows))
        (when-let ((seat (plist-get row :seat)))
          (push (concat (plist-get row :dot) seat) dots)))
      (mapconcat (function identity) (nreverse dots) " ")))
  (free-agent-bus-status)
  (princ (walk-status-dots)) (terpri)
  (free-agent-bus-status-toggle-yellow)
  (princ (walk-status-dots)) (terpri)
  (free-agent-bus-status-toggle-yellow)
  (princ (walk-status-dots)) (terpri)
  (kill-current-buffer))')
assert_eq "dots read busy/ready/parked, hiding takes the parked, showing restores it" \
  $'🟣busy 🟢fresh 🟡parked\n🟣busy 🟢fresh\n🟣busy 🟢fresh 🟡parked' "$out"

finish
