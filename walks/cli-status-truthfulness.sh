#!/usr/bin/env bash
# cli-status-truthfulness — the live roster answers for silence, not the process table.
#
# The 30 Oct wound: carafe, dripper and tamper showed 🟢 idle 22 hours after
# their processes vanished, because idle heartbeats were exempt from the stale
# check and pids get recycled. e63c207 made idle age out like every other
# state. This walk pins: old idle + live pid reads 🟡 stale with the pid
# named; old idle + dead pid reads 🔴 crashed; fresh idle stays 🟢; a poster
# with no heartbeat at all still gets its own ⚪ row.

source "$(dirname "$0")/lib.sh"

step "a root with four seats in four different silences"
new_root status
MYPID=$$
TWO_HOURS_AGO=$(date -u -v-2H +%Y-%m-%dT%H:%M:%S.000000Z)
NOW=$(date -u +%Y-%m-%dT%H:%M:%S.000000Z)
printf 'idle %s %s' "$TWO_HOURS_AGO" "$MYPID" >"$ROOT/.hb-stale"
printf 'idle %s 99999999' "$TWO_HOURS_AGO" >"$ROOT/.hb-crashed"
printf 'idle %s %s' "$NOW" "$MYPID" >"$ROOT/.hb-fresh"
say ghost public "a post from a seat with no heartbeat"

step "status --live reads each silence honestly"
live=$("$FA" bus status -r "$ROOT" --live --depth 1 2>&1)
assert_eq "old idle with a live pid reads stale, and names the pid" \
  "1" "$(printf '%s' "$live" | grep -c "🟡 stale — idle .*; pid $MYPID alive — stale?" || true)"
assert_eq "the stale seat is never green" \
  "0" "$(printf '%s' "$live" | grep -c '^🟢 stale ' || true)"
assert_eq "old idle with a dead pid reads crashed" \
  "1" "$(printf '%s' "$live" | grep -c '🔴 crashed — pid 99999999 is not running' || true)"
assert_eq "fresh idle is still green" \
  "1" "$(printf '%s' "$live" | grep -c "🟢 fresh — idle .*ago" || true)"
assert_eq "a poster with no heartbeat gets its own row" \
  "1" "$(printf '%s' "$live" | grep -c '⚪ ghost — no heartbeat' || true)"

finish
