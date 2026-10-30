#!/usr/bin/env bash
# cli-verify-addressing-reach — what "public" means at delivery time.
#
# The CLI's own refusal text calls an omitted --to "a public post", and
# deliversTo disagrees: it answers False for an empty audience, so a post with
# no recipient flag reaches no seat (circuits-agent Circuit/Agent.hs, the
# empty-audience clause). This walk measures that reach.
#
# Two ways to write a recipient list, two policies. Flags validate: `say` and
# `post` refuse an empty name with exit 1 and write nothing. Raw JSON through
# `bus post` is the caller's own statement and is not validated, so the
# to: [""] shape is still reachable — and still reaches nobody, because
# deliversTo compares names and no seat is called "".

source "$(dirname "$0")/lib.sh"

step "one addressed post and one with no recipient flag"
new_root reach
say alice bob "addressed"
say alice - "no recipient flag"
assert_eq "the addressed post names bob" '["bob"]' "$(field 0 to)"
assert_eq "the unflagged post has an empty audience" '[]' "$(field 1 to)"
assert_eq "and the log holds exactly two" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "say refuses an empty recipient before anything is written"
out=$(printf 'x' | "$FA" bus say -r "$ROOT" --from alice --to "" 2>&1); rc=$?
assert_eq "an empty recipient exits 1" "1" "$rc"
assert_eq "and names the repair" "1" \
  "$(printf '%s' "$out" | grep -c 'say: empty recipient name — omit --to to post unaddressed' || true)"
assert_eq "nothing was appended" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "post refuses it too, under its own name"
out=$(printf '' | "$FA" bus post -r "$ROOT" --from alice --body "x" --to "" 2>&1); rc=$?
assert_eq "post with an empty recipient exits 1" "1" "$rc"
assert_eq "and says post, not say" "1" \
  "$(printf '%s' "$out" | grep -c 'post: empty recipient name — omit --to to post unaddressed' || true)"
assert_eq "and writes nothing" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "one empty recipient poisons a list that also names a real one"
out=$(printf 'x' | "$FA" bus say -r "$ROOT" --from alice --to bob --to "" 2>&1); rc=$?
assert_eq "a mixed recipient list exits 1" "1" "$rc"
assert_eq "and writes nothing either" "2" "$(count_lines cat "$ROOT/log.jsonl")"

step "raw JSON is not validated, so the empty name survives as a field shape"
js='{"from":"alice","to":[""],"thread":[],"body":"empty name via json"}'
out=$(printf '%s' "$js" | "$FA" bus post -r "$ROOT" 2>&1); rc=$?
assert_eq "a JSON post with an empty name is accepted" "0" "$rc"
assert_eq "and the log holds three" "3" "$(count_lines cat "$ROOT/log.jsonl")"
assert_eq "the empty recipient is on the record" '[""]' "$(field 2 to)"
assert_eq "flags alongside stdin are refused rather than merged" "1" \
  "$(printf '%s' "$js" | "$FA" bus post -r "$ROOT" --from alice 2>&1 | grep -c 'post requires either --from and --body flags or JSON on stdin' || true)"
assert_eq "and that refusal wrote nothing" "3" "$(count_lines cat "$ROOT/log.jsonl")"

step "reach, read through the delivery view"
out=$("$FA" bus read -r "$ROOT" bob --depth 0)
assert_eq "exactly one post reaches bob" "1" "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "and it is post 0, the addressed one" "0" "$(printf '%s' "$out" | awk '{print $1}')"
assert_eq "an omitted --to is delivered to no seat" "0" \
  "$(printf '%s' "$out" | grep -c 'no recipient flag' || true)"
assert_known "the JSON post with one empty name reaches nobody either" "0" \
  "$(printf '%s' "$out" | grep -c 'empty name via json' || true)" \
  "deliversTo compares names, and no seat is called the empty string; the JSON form is deliberately unvalidated, so this shape stays reachable and stays unreachable mail"

step "inbox agrees with read"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "inbox shows bob the same single post" "1" \
  "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"

step "the unflagged say is still accepted without a word"
printf 'x' | "$FA" bus say -r "$ROOT" --from alice >/dev/null 2>&1
assert_eq "an unflagged say exits 0" "0" "$?"
assert_eq "and the log now holds four" "4" "$(count_lines cat "$ROOT/log.jsonl")"

finish
