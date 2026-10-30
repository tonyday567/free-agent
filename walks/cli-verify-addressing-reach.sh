#!/usr/bin/env bash
# cli-verify-addressing-reach — what "public" means at delivery time.
#
# The CLI's own refusal text calls an omitted --to "a public post", and
# deliversTo disagrees: it answers False for an empty audience, so a post with
# no recipient flag reaches no seat (circuits-agent Circuit/Agent.hs, the
# empty-audience clause). This walk measures that reach.
#
# The empty-name form, --to "", is no longer a post at all: say refuses it at
# write time (exit 1, "empty recipient name"), so there is no field shape to
# measure and nothing is written. That refusal is pinned here rather than argued.

source "$(dirname "$0")/lib.sh"

step "one addressed post and one with no recipient flag"
new_root reach
say alice bob "addressed"
say alice - "no recipient flag"
assert_eq "the addressed post names bob" '["bob"]' "$(field 0 to)"
assert_eq "the unflagged post has an empty audience" '[]' "$(field 1 to)"
assert_eq "and the log holds exactly two" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "the empty recipient is refused before anything is written"
out=$(printf 'x' | "$FA" bus say -r "$ROOT" --from alice --to "" 2>&1); rc=$?
assert_eq "an empty recipient exits 1" "1" "$rc"
assert_eq "and names the repair" "1" \
  "$(printf '%s' "$out" | grep -c 'say: empty recipient name — omit --to for a public post' || true)"
assert_eq "nothing was appended" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "one empty recipient poisons a post that also names a real one"
out=$(printf 'x' | "$FA" bus say -r "$ROOT" --from alice --to bob --to "" 2>&1); rc=$?
assert_eq "a mixed recipient list exits 1" "1" "$rc"
assert_eq "and writes nothing either" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "reach, read through the delivery view"
out=$("$FA" bus read -r "$ROOT" bob --depth 0)
assert_eq "exactly one post reaches bob" "1" "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "and it is post 0, the addressed one" "0" "$(printf '%s' "$out" | awk '{print $1}')"
assert_known "the unflagged post reaches nobody at all" "0" \
  "$(printf '%s' "$out" | grep -c 'no recipient flag' || true)" \
  "deliversTo answers False for an empty audience, so 'public' is not a delivery state — the refusal text still calls an omitted --to a public post"

step "inbox agrees with read"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "inbox shows bob the same single post" "1" \
  "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"

step "the unflagged say is still accepted without a word"
printf 'x' | "$FA" bus say -r "$ROOT" --from alice >/dev/null 2>&1
assert_eq "an unflagged say exits 0" "0" "$?"
assert_eq "and the log now holds three" "3" "$(count_lines cat "$ROOT/log.jsonl")"

finish
