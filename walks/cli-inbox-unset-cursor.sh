#!/usr/bin/env bash
# cli-inbox-unset-cursor — the wound behind the runner's inbox reading zero.
#
# Measured 31 Oct: a name with mail but no cursor file receives nothing, silently
# and with exit 0. The unread computation is "id >= cursor", so an absent cursor
# is not "everything is new" — it is "nothing is new". The runner lost 24 minutes
# and most of a context to exactly this on 30 Oct (bus 233).
#
# The deck's fresh-inbox path was fixed to read from the first post (free-agent
# 1f78f6a); the CLI's `inbox` was not. That divergence is the point of the walk.

source "$(dirname "$0")/lib.sh"

step "mail for bob, and no cursor file"
new_root unset
say alice bob "the first thing bob should see"
say alice bob "and the second"
assert_eq "the log holds both" "0,1" "$(ids_of_to bob)"
[ -e "$ROOT/.cursor-bob" ] && note "a cursor exists before we start" || note "no cursor file, as intended"

step "inbox bob with no cursor"
out=$("$FA" bus inbox -r "$ROOT" bob --depth 0)
assert_known "a name with mail but no cursor gets nothing" \
  "0" "$(printf '%s' "$out" | grep -c . || true)" \
  "unread is id >= cursor, so an absent cursor reads as caught up; the deck reads it as everything-new"

step "and exits 0, so nothing warns the caller"
"$FA" bus inbox -r "$ROOT" bob --depth 0 >/dev/null 2>&1
assert_known "the empty read is not an error" "0" "$?" \
  "a silent zero is why the runner's inbox looked empty rather than broken"

step "mark-read on a missing cursor writes nothing"
"$FA" bus inbox -r "$ROOT" bob --mark-read --depth 0 >/dev/null 2>&1
assert_known "no cursor is created" "absent" \
  "$([ -e "$ROOT/.cursor-bob" ] && echo present || echo absent)" \
  "nothing printed means nothing to advance past, so the state stays absent"

step "the explicit fix that works today: set the cursor first"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "with cursor 0 the same mail appears" "2" "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"

finish
