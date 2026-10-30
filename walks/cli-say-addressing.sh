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

step "omitting --to posts with no recipient, which is not the same as refused"
say alice - "no recipient flag at all"
assert_known "missing --to writes an empty recipient list" '[]' "$(field 1 to)" \
  "the card says this is refused; it is not. Nothing is refused, and nothing is delivered: read NAME and inbox NAME both answer False for to: []"
assert_eq "and --thread is empty for a root post" '[]' "$(field 1 thread)"

step "an empty --to is refused, and says which way to go instead"
out=$("$FA" bus say -r "$ROOT" --from alice --to "" "an empty recipient" 2>&1); rc=$?
assert_eq '--to "" exits 1' "1" "$rc"
assert_eq "with a named refusal" "🔴 say: empty recipient name — omit --to to post unaddressed (visible in the log, delivered to no seat)" "$out"
assert_eq "and it writes nothing" "2" "$(wc -l <"$ROOT/log.jsonl" | tr -d ' ')"

step "an empty name poisons a list that also names a real recipient"
out=$("$FA" bus say -r "$ROOT" --from alice --to bob --to "" "mixed" 2>&1); rc=$?
assert_eq "a mixed --to list exits 1" "1" "$rc"
assert_eq "with the same refusal" "🔴 say: empty recipient name — omit --to to post unaddressed (visible in the log, delivered to no seat)" "$out"
assert_eq "and the post never lands, so bob's mail is untouched" "2" "$(wc -l <"$ROOT/log.jsonl" | tr -d ' ')"

step "measured: neither of those two reaches bob, and the addressed post does"
# Reach is measured through the two CLI views, at an explicit depth so a change to a
# default cannot quietly change what this asserts. Both views filter through
# `filterSince` -> `deliversTo` (Bus/Cli.hs:765), used by runRead (Cli.hs:518) and
# runInbox (Cli.hs:532). The seat delivery path reimplements the same predicate in its
# own filters (Bus/File.hs:428, 433), so this claim transfers by shared predicate rather
# than by end-to-end measure. `bus watch` is a third consumer of `deliversTo`
# (Bus/Cli.hs:491) if that ever needs walking for real; it carries no `--depth`, that
# flag belonging to `read` and `inbox` - there is no `bus log`.
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "bob's readable mail is the addressed post alone" "0" \
  "$("$FA" bus read -r "$ROOT" bob --depth 2 | json_ids)"
assert_eq "and inbox agrees" "0" \
  "$("$FA" bus inbox -r "$ROOT" bob --depth 2 | json_ids)"

step "ancestry attaches to a post that exists, and a dangling parent is refused"
printf 'with ancestry' | "$FA" bus say -r "$ROOT" --from alice --to bob --thread 0 >/dev/null
assert_eq "--thread 0 records parent 0" '[0]' "$(field 2 thread)"
out=$(printf 'dangling ancestry' | "$FA" bus say -r "$ROOT" --from alice --to bob --thread 99 2>&1); rc=$?
assert_eq "--thread 99 is refused" "1" "$rc"
assert_eq "with a named refusal" "🔴 say: no post with id 99 — threads attach to an existing post" "$out"
assert_eq "and it writes nothing" "3" "$(wc -l <"$ROOT/log.jsonl" | tr -d ' ')"

step "and the read of a dangling id fails loudly too"
out=$("$FA" bus read -r "$ROOT" --thread 99 --depth 0 2>&1)
assert_eq "read --thread 99 exits 1" "1" "$?"
assert_eq "with a named refusal" "🔴 no post with id 99" "$out"

step "a thread read returns the root and its replies"
assert_eq "thread 0 has two members" "2" "$(count_lines "$FA" bus read -r "$ROOT" --thread 0 --depth 0)"

finish
