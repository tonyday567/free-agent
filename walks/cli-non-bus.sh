#!/usr/bin/env bash
# cli-non-bus — a directory with no log.jsonl is not a bus.
#
# Every surface must refuse a root that is not a bus, with the same
# message — one refusal, noBusMessage, ending in the repair hint — and a
# non-zero exit. read was the exception (quiet success, 31 Oct) and status
# was the half-case (same words, hint dropped, also 31 Oct); both now refuse
# identically, and the walk pins the uniform message.

source "$(dirname "$0")/lib.sh"

step "a directory whose log.jsonl was removed after seeding"
new_root nonbus
rm "$ROOT/log.jsonl"

step "say is refused"
out=$(printf 'hi' | "$FA" bus say -r "$ROOT" --from a --to b 2>&1); rc=$?
assert_eq "say exits non-zero" "1" "$rc"
assert_eq "say names the reason" \
  "1" "$(printf '%s' "$out" | grep -c 'no log.jsonl; not a bus' || true)"

step "inbox is refused"
out=$("$FA" bus inbox -r "$ROOT" a 2>&1); rc=$?
assert_eq "inbox exits non-zero" "1" "$rc"
assert_eq "inbox names the reason" \
  "1" "$(printf '%s' "$out" | grep -c 'no log.jsonl; not a bus' || true)"

step "status is refused"
out=$("$FA" bus status -r "$ROOT" 2>&1); rc=$?
assert_eq "status exits non-zero" "1" "$rc"
assert_eq "status names the reason" \
  "1" "$(printf '%s' "$out" | grep -c 'no log.jsonl; not a bus' || true)"
assert_eq "status names the repair, the way every surface does" \
  "1" "$(printf '%s' "$out" | grep -c 'Set FREE_AGENT_BUS_ROOT or use --root' || true)"

step "read is refused, the way the other three are"
out=$("$FA" bus read -r "$ROOT" a --depth 1 2>&1); rc=$?
assert_eq "read exits non-zero" "1" "$rc"
assert_eq "read names the reason" \
  "1" "$(printf '%s' "$out" | grep -c 'no log.jsonl; not a bus' || true)"

step "read --thread is refused the same way"
out=$("$FA" bus read -r "$ROOT" --thread 0 --depth 1 2>&1); rc=$?
assert_eq "read --thread exits non-zero" "1" "$rc"
assert_eq "read --thread names the reason" \
  "1" "$(printf '%s' "$out" | grep -c 'no log.jsonl; not a bus' || true)"

finish
