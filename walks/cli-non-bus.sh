#!/usr/bin/env bash
# cli-non-bus — a directory with no log.jsonl is not a bus.
#
# Every surface must refuse a root that is not a bus, with the same
# message and a non-zero exit. read was the exception — it quietly answered
# nothing and exited 0 — and this walk pinned that as a known until the
# refusal was wired in (31 Oct). All four surfaces refuse here now.

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
