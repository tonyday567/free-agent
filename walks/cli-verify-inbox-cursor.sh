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
#     Untested by the four walks.
#   * `status` reports unread as maxId - cursor + 1, which counts other people's
#     mail. It disagrees with `inbox` on the same root.

source "$(dirname "$0")/lib.sh"

step "mark-read must not jump past mail addressed to someone else"
new_root cursor
say alice bob "bob's first"
say alice bob "bob's second"
say alice carol "carol's, later in the log"
assert_eq "post 2 is carol's, so the log tip is 2" '["carol"]' "$(field 2 to)"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
out=$("$FA" bus inbox -r "$ROOT" bob --mark-read --depth 0)
assert_eq "bob's unread is exactly two posts" "2" "$(printf '%s' "$out" | grep -c . || true)"
assert_eq "cursor is highest printed + 1 (2), not log tip + 1 (3)" "2" "$(cat "$ROOT/.cursor-bob")"

step "the same absent-cursor default is reachable through read --cursor NAME"
new_root read-cursor
say alice bob "one"
say alice bob "two"
out=$("$FA" bus read -r "$ROOT" bob --cursor bob --depth 0)
assert_known "read --cursor NAME with no cursor file prints nothing" "0" \
  "$(printf '%s' "$out" | grep -c . || true)" \
  "readCursor's cold-boot default is the log tip, so the cursor flag is silent for a fresh name"
assert_eq "while read NAME with no --since prints both" "2" \
  "$(count_lines "$FA" bus read -r "$ROOT" bob --depth 0)"

step "a root with no log: inbox refuses, read does not"
mkdir -p "$SCRATCH/$WALK/no-bus"
empty="$SCRATCH/$WALK/no-bus"
"$FA" bus inbox -r "$empty" bob --depth 0 >/dev/null 2>&1
assert_eq "inbox exits 1" "1" "$?"
"$FA" bus read -r "$empty" bob --depth 0 >/dev/null 2>&1
assert_known "read NAME on a non-bus root exits 0 with no output" "0" "$?" \
  "runRead skips the missing file and never calls requireBus, unlike inbox and say"

step "status counts posts after the cursor, not posts addressed to the name"
new_root status-count
say alice bob "for bob"
say alice carol "for carol"
say alice carol "for carol again"
"$FA" bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "inbox says bob has one" "1" "$(count_lines "$FA" bus inbox -r "$ROOT" bob --depth 0)"
row=$("$FA" bus status -r "$ROOT" 2>&1 | grep 'bob @' | sed 's/^ *//' || true)
assert_known "status says three" "bob @0 (3 unread)" "$row" \
  "Behind is maxId - cursor + 1, so other people's posts count as the name's unread"

finish
