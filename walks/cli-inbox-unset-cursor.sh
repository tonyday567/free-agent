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
#
# Repaired 31 Oct: the CLI's inbox and read now take the mail view of the
# cursor — a missing cursor means the name has seen nothing, so everything
# addressed to it is unread, and the first --mark-read bootstraps the cursor
# file. The assertions below pin the repaired behaviour; the wound record
# above stays as history.

source "$(dirname "$0")/lib.sh"

step "mail for bob, and no cursor file"
new_root unset
say alice bob "the first thing bob should see"
say alice bob "and the second"
assert_eq "the log holds both" "0,1" "$(ids_of_to bob)"
[ -e "$ROOT/.cursor-bob" ] && note "a cursor exists before we start" || note "no cursor file, as intended"

step "inbox bob with no cursor"
out=$("$FA" bus inbox -r "$ROOT" bob --depth 0)
assert_eq "a name with mail but no cursor sees everything addressed to it" \
  "2" "$(printf '%s' "$out" | grep -c . || true)"

step "and exits 0, delivery without ceremony"
"$FA" bus inbox -r "$ROOT" bob --depth 0 >/dev/null 2>&1
assert_eq "the read is not an error" "0" "$?"

step "and later mail keeps arriving"
say alice bob "posted after the first read"
assert_eq "a second post appears on the next read" "3" \
  "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"

step "and --mark-read bootstraps the cursor"
"$FA" bus inbox -r "$ROOT" bob --mark-read --depth 0 >/dev/null 2>&1
assert_eq "the cursor file is created" "present" \
  "$([ -e "$ROOT/.cursor-bob" ] && echo present || echo absent)"
assert_eq "at the id after the highest post printed" "3" \
  "$(cat "$ROOT/.cursor-bob")"

step "an explicit cursor still wins: set it and only newer mail appears"
"$FA" bus cursor set -r "$ROOT" bob 2 >/dev/null
assert_eq "only the post at or after the cursor" "2" \
  "$("$FA" bus inbox -r "$ROOT" bob --depth 0 | dot_ids)"

finish
