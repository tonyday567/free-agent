#!/usr/bin/env bash
# cli-say-addressing — who a post is for, and the three ways that goes wrong.
#
# The task card records "a missing --to is refused, an empty one is public".
# Measured 31 Oct, both halves are wrong, and differently:
#   --to absent   posts with to: []      — public, exit 0, no refusal
#   --to ""       posts with to: [""]    — a recipient list holding an empty name,
#                                         which no seat will ever read
# A public post is fine when it is meant. The defect is that the two are
# indistinguishable from the command line and one of them is unreachable mail.

source "$(dirname "$0")/lib.sh"

step "a root and a public post"
new_root addressing
say alice bob "addressed"
assert_eq "an addressed post names its recipient" '["bob"]' "$(field 0 to)"

step "omitting --to posts publicly rather than refused"
say alice - "no recipient flag at all"
assert_known "missing --to becomes a public post" '[]' "$(field 1 to)" \
  "the card says this is refused; it is not, it is public"
assert_eq "and --thread is empty for a root post" '[]' "$(field 1 thread)"

step "an empty --to is not the same thing as an omitted one"
say alice "" "an empty recipient"
assert_known '--to "" writes a recipient list holding an empty name' '[""]' "$(field 2 to)" \
  "public is to: [], so this post is addressed to nobody and delivered to nobody"

step "ancestry is carried verbatim, including ids that do not exist"
printf 'with ancestry' | "$FA" bus say -r "$ROOT" --from alice --to bob --thread 0 >/dev/null
assert_eq "--thread 0 records parent 0" '[0]' "$(field 3 thread)"
printf 'dangling ancestry' | "$FA" bus say -r "$ROOT" --from alice --to bob --thread 99 >/dev/null
assert_known "--thread 99 is accepted though no post 99 exists" '[99]' "$(field 4 thread)" \
  "the log tolerates a dangling parent; the read is where it fails"

step "and the read of that dangling id fails loudly, which is the right half"
out=$("$FA" bus read -r "$ROOT" --thread 99 --depth 0 2>&1)
assert_eq "read --thread 99 exits 1" "1" "$?"
assert_eq "with a named refusal" "🔴 no post with id 99" "$out"

step "a thread read returns the root and its replies"
assert_eq "thread 0 has two members" "2" "$(count_lines "$FA" bus read -r "$ROOT" --thread 0 --depth 0)"

finish
