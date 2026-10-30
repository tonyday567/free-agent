#!/usr/bin/env bash
# cli-inbox-flow — the happy path of unread mail: cursor, delivery, mark-read.
#
# The semantics this pins down, measured against the installed CLI:
#   unread = ids addressed to NAME that are >= the cursor, cursor inclusive;
#   --mark-read sets the cursor to the highest id printed, plus one;
#   a name caught up prints nothing and exits 0.

source "$(dirname "$0")/lib.sh"

step "a fresh root, two posts to bob"
new_root flow
say alice bob "first"
say alice bob "second"
assert_eq "post 0 is addressed to bob" '["bob"]' "$(field 0 to)"

step "cursor set to 0, inbox bob"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "cursor get reads back 0" "0" "$("$FA" bus cursor get -r "$ROOT" bob)"
assert_eq "inbox prints both posts" "2" "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"

step "mark-read advances past the highest printed id"
"$FA" bus inbox -r "$ROOT" bob --mark-read --depth 0 >/dev/null
assert_eq "cursor is highest printed + 1" "2" "$(cat "$ROOT/.cursor-bob")"

step "caught up prints nothing, and says so by exiting 0"
assert_eq "inbox is empty" "0" "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"
"$FA" bus inbox -r "$ROOT" bob --depth 0 >/dev/null
assert_eq "and exits 0" "0" "$?"

step "a newer post is delivered, and only the newer one"
say alice bob "third"
out=$("$FA" bus inbox -r "$ROOT" bob --depth 0)
assert_eq "one line printed" "1" "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "it is post 2" "2" "$(printf '%s' "$out" | awk '{print $1}')"
assert_eq "bob's mail is ids 0,1,2" "0,1,2" "$(ids_of_to bob)"

finish
