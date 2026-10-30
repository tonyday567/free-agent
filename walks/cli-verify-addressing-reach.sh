#!/usr/bin/env bash
# cli-verify-addressing-reach — what "public" means at delivery time.
#
# cli-say-addressing records an omitted --to as "a public post" and --to "" as
# "unreachable mail", as though the two differed in kind. At the delivery
# surface they do not: deliversTo answers False for to: [] and for to: [""]
# alike (circuits-agent Circuit/Agent.hs, the empty-audience and empty-name
# clauses), so a post with no recipient flag reaches no seat. The field shapes
# differ; the reach does not. This walk measures reach.

source "$(dirname "$0")/lib.sh"

step "three posts: addressed, no flag, empty recipient"
new_root reach
say alice bob "addressed"
say alice - "no recipient flag"
say alice "" "empty recipient"
assert_eq "the addressed post names bob" '["bob"]' "$(field 0 to)"
assert_eq "the unflagged post has an empty audience" '[]' "$(field 1 to)"
assert_eq "the empty-recipient post has one empty name" '[""]' "$(field 2 to)"

step "reach, read through the delivery view"
out=$("$FA" bus read -r "$ROOT" bob --depth 0)
assert_eq "exactly one post reaches bob" "1" "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "and it is post 0, the addressed one" "0" "$(printf '%s' "$out" | awk '{print $1}')"
assert_known "the unflagged post reaches nobody at all" "0" \
  "$(printf '%s' "$out" | grep -c 'no recipient flag' || true)" \
  "deliversTo answers False for an empty audience, so 'public' is not a delivery state"

step "inbox agrees with read"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "inbox shows bob the same single post" "1" \
  "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"

step "the CLI accepted the unreachable posts without a word"
printf 'x' | "$FA" bus say -r "$ROOT" --from alice >/dev/null 2>&1
assert_eq "an unflagged say exits 0" "0" "$?"
assert_eq "and the log now holds four" "4" "$(count_lines cat "$ROOT/log.jsonl")"

finish
