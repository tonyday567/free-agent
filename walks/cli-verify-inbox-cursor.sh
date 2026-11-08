#!/usr/bin/env bash
# cli-verify-inbox-cursor — verification of the inbox, mark-read and status claims.
#
# Written by shen while verifying yin's four walks (free-agent 5855320), the
# same harness, a different reading. Three gaps it closes:
#
#   * cli-inbox-flow's mark-read step cannot tell "highest printed id + 1" from
#     "log tip + 1": no post follows the last one addressed to the reader, so
#     both answers are 2. Here a post for carol lands after bob's, and the
#     answers differ (2 versus 3).
#   * the absent-cursor default is not an inbox quirk: `read --cursor NAME` reads
#     the same cursor through the same function, and is silent for a fresh name.
#     Untested by the four walks. Repaired 31 Oct: the mail view treats a
#     missing cursor as nothing-seen (free-agent 7abef9d).
#   * `status` reports unread as maxId - cursor + 1, which counts other people's
#     mail. It disagrees with `inbox` on the same root. Repaired 31 Oct:
#     the figure is now the mail count — what `inbox` prints for the name.
#   * `read` on a root with no log.jsonl answered silently, exit 0, while inbox,
#     say and status refused. Repaired 31 Oct: read and its thread view now
#     refuse with the root named (free-agent ced433d).

source "$(dirname "$0")/lib.sh"

step "mark-read must not jump past mail addressed to someone else"
new_root cursor
post alice bob "bob's first"
post alice bob "bob's second"
post alice carol "carol's, later in the log"
assert_eq "post 2 is carol's, so the log tip is 2" '["carol"]' "$(field 2 to)"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
out=$("$FA" bus inbox -r "$ROOT" bob --mark-read --depth 0)
assert_eq "bob's unread is exactly two posts" "2" "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "cursor is highest printed + 1 (2), not log tip + 1 (3)" "2" "$(cat "$ROOT/.cursor-bob")"

step "a missing cursor means the name has seen nothing"
new_root read-cursor
post alice bob "one"
post alice bob "two"
out=$("$FA" bus inbox -r "$ROOT" bob --depth 0)
assert_eq "inbox delivers the whole history to a fresh name" "2" \
  "$(printf '%s' "$out" | grep -c . || true)"
out=$("$FA" bus read -r "$ROOT" bob --cursor bob --depth 0)
assert_eq "read --cursor NAME agrees" "2" \
  "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "and read NAME with no --since still prints both" "2" \
  "$(count_lines "$FA" bus read -r "$ROOT" bob --depth 0)"

step "a root with no log: every view refuses it, read included"
# Not new_root: that writes an empty log.jsonl, and this step needs a root with
# no log at all. Same per-run pid suffix, for the same concurrency reason.
empty="$SCRATCH/$WALK/no-bus.$$"
rm -rf "$empty"
mkdir -p "$empty"
"$FA" bus inbox -r "$empty" bob --depth 0 >/dev/null 2>&1
assert_eq "inbox exits 1" "1" "$?"
out=$("$FA" bus read -r "$empty" bob --depth 0 2>&1); rc=$?
assert_eq "read NAME exits 1" "1" "$rc"
assert_eq "and names the root and the repair" "1" \
  "$(printf '%s' "$out" | grep -c 'no log.jsonl; not a bus. Set FREE_AGENT_BUS_ROOT or use --root' || true)"
"$FA" bus read -r "$empty" --thread 0 --depth 0 >/dev/null 2>&1
assert_eq "the thread view refuses it too" "1" "$?"

step "status's unread figure is the mail count, as inbox"
new_root status-count
post alice bob "for bob"
post alice carol "for carol"
post alice carol "for carol again"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "inbox says bob has one" "1" "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"
row=$("$FA" bus status -r "$ROOT" --depth 1 2>&1 | grep 'bob @' | sed 's/^ *//' || true)
assert_eq "status agrees: one unread, not the log span" "bob @0 (1 unread)" "$row"

finish
