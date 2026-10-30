#!/usr/bin/env bash
# cli-cursor — the read marker: where a name resumes.
#
# The cursor is a per-name PostId. get on a missing file is not an error —
# it is the id that would follow the last post, which is exactly where a
# fresh seat should start. set/get round-trips, and a cursor past the end
# of the log is stored verbatim (the seat simply reads as caught up).

source "$(dirname "$0")/lib.sh"

step "get on a missing cursor is the next id, not an error"
new_root cursor
say a b "one"
say a b "two"
out=$("$FA" bus cursor get -r "$ROOT" nadia 2>&1); rc=$?
assert_eq "get exits 0 without a cursor file" "0" "$rc"
assert_eq "and answers the id that would follow the last post" "2" "$out"

step "set/get round trip"
"$FA" bus cursor set -r "$ROOT" nadia 99 >/dev/null
assert_eq "a cursor past the end is stored" "99" "$("$FA" bus cursor get -r "$ROOT" nadia)"
"$FA" bus cursor set -r "$ROOT" nadia 1 >/dev/null
assert_eq "and set back into the log" "1" "$("$FA" bus cursor get -r "$ROOT" nadia)"
assert_eq "the file holds the value" "1" "$(cat "$ROOT/.cursor-nadia")"

step "an empty log answers 0"
new_root cursor-empty
assert_eq "nothing to resume into" "0" "$("$FA" bus cursor get -r "$ROOT" nadia)"

finish
