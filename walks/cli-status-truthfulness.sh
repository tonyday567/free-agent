#!/usr/bin/env bash
# cli-status-truthfulness — one census reads fab state, unread mail, and membership.
#
# Live pids separate ready, working, and parked from crashed. Explicit death is
# retired black, never crashed red. No heartbeat is non-fab white. Rows carry no
# repeated state prose or raw cursor; depth 2 remains the diagnostic drill-down.

source "$(dirname "$0")/lib.sh"

step "a root with seven names in six status circles"
new_root status
MYPID=$$
TWO_HOURS_AGO=$(date -u -v-2H +%Y-%m-%dT%H:%M:%S.000000Z)
TWO_AHEAD=$(date -u -v+2H +%Y-%m-%dT%H:%M:%S.000000Z)
NOW=$(date -u +%Y-%m-%dT%H:%M:%S.000000Z)
printf 'idle %s %s' "$TWO_HOURS_AGO" "$MYPID" >"$ROOT/.hb-parked"
printf 'idle %s 99999999' "$TWO_HOURS_AGO" >"$ROOT/.hb-crashed"
printf 'idle %s %s' "$NOW" "$MYPID" >"$ROOT/.hb-fresh"
printf 'working %s %s' "$NOW" "$MYPID" >"$ROOT/.hb-busy"
printf 'idle %s %s' "$TWO_AHEAD" "$MYPID" >"$ROOT/.hb-skewed"
printf 'dead %s %s' "$NOW" "$MYPID" >"$ROOT/.hb-retired"
post ghost - "a post from a participant with no heartbeat"

step "bus status reads one unified snapshot"
status=$("$FA" bus status -r "$ROOT" --depth 1 2>&1)
assert_eq "the census has exact counts and the last-post anchor" \
  "1" "$(printf '%s' "$status" | grep -c '^🟣 1 🟢 2 🟡 1 🔴 1 ⚫ 1 ⚪ 1 | last \[#0\]$' || true)"
assert_eq "idle past the hour with a live pid is parked" \
  "1" "$(printf '%s' "$status" | grep -c '^🟡 parked (0) no posts$' || true)"
assert_eq "a working seat is purple" \
  "1" "$(printf '%s' "$status" | grep -c '^🟣 busy (0) no posts$' || true)"
assert_eq "old idle with a dead pid is crashed red" \
  "1" "$(printf '%s' "$status" | grep -c '^🔴 crashed (0) no posts$' || true)"
assert_eq "fresh idle is ready green" \
  "1" "$(printf '%s' "$status" | grep -c '^🟢 fresh (0) no posts$' || true)"
assert_eq "explicit death is retired black, not crashed red" \
  "1" "$(printf '%s' "$status" | grep -c '^⚫ retired (0) no posts$' || true)"
assert_eq "a participant with no heartbeat is non-fab white" \
  "1" "$(printf '%s' "$status" | grep -c '^⚪ ghost (0) post \[#0\]$' || true)"
assert_eq "a future heartbeat still reads ready without printing negative age" \
  "1" "$(printf '%s' "$status" | grep -c '^🟢 skewed (0) no posts$' || true)"
assert_eq "state prose and the old cursor summary are gone" \
  "0" "$(printf '%s' "$status" | grep -Ec ' — idle| — working| — retired|no heartbeat|cursor summary|cursor \[#' || true)"

step "the split live APIs are retired"
"$FA" bus live -r "$ROOT" >/dev/null 2>&1
assert_eq "bus live is no longer a command" "1" "$?"
"$FA" bus status -r "$ROOT" --live >/dev/null 2>&1
assert_eq "status --live is no longer a flag" "1" "$?"

finish
