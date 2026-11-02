#!/usr/bin/env bash
# cli-status-truthfulness — the live roster reads loop state: busy, ready, parked,
# crashed, unknown; age decides only where readiness stops.
#
# The 30 Oct wound: carafe, dripper and tamper showed 🟢 idle 22 hours after
# their processes vanished, because idle heartbeats were exempt from the stale
# check and pids get recycled. e63c207 aged idle out. 3 Nov moved the boundary
# back where readiness can still be read: under the hour an idle seat stays 🟢,
# past it the dot parks 🟡, and the crashed and unknown readings keep naming
# the failures. This walk pins: idle 2h + live pid reads 🟡 parked; idle 2h +
# dead pid reads 🔴 crashed; fresh idle stays 🟢; fresh working reads 🟣 busy;
# a poster with no heartbeat at all still gets its own ⚪ row.

source "$(dirname "$0")/lib.sh"

step "a root with six seats in six different states"
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
say ghost public "a post from a seat with no heartbeat"

step "status --live reads each silence honestly"
live=$("$FA" bus status -r "$ROOT" --live --depth 1 2>&1)
assert_eq "idle past the hour with a live pid reads parked yellow" \
  "1" "$(printf '%s' "$live" | grep -c '^🟡 parked — idle 2h ago' || true)"
assert_eq "the parked seat is never green" \
  "0" "$(printf '%s' "$live" | grep -c '^🟢 parked ' || true)"
assert_eq "the parked reading carries no staleness note" \
  "0" "$(printf '%s' "$live" | grep -c '^🟡 parked .*stale?' || true)"
assert_eq "a working seat reads busy purple" \
  "1" "$(printf '%s' "$live" | grep -c '^🟣 busy — working .*ago' || true)"
assert_eq "the busy seat is never yellow" \
  "0" "$(printf '%s' "$live" | grep -c '^🟡 busy ' || true)"
assert_eq "old idle with a dead pid reads crashed" \
  "1" "$(printf '%s' "$live" | grep -c '🔴 crashed — pid 99999999 is not running' || true)"
assert_eq "fresh idle is still green" \
  "1" "$(printf '%s' "$live" | grep -c "🟢 fresh — idle .*ago" || true)"
assert_eq "a poster with no heartbeat gets its own row" \
  "1" "$(printf '%s' "$live" | grep -c '⚪ ghost — no heartbeat' || true)"
assert_known "a future heartbeat with a live pid reads green, with a negative age" \
  "1" "$(printf '%s' "$live" | grep -c '🟢 skewed — idle -[0-9]*s ago' || true)" \
  "runStatusLive subtracts the timestamp from now without clamping, so a clock-skewed seat sits in the roster looking healthy for as long as the skew lasts — measured 31 Oct: idle -7200s ago for a heartbeat stamped two hours ahead; clamp the negative and say why"

finish
