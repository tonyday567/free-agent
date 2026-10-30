#!/usr/bin/env bash
# cli-non-bus — a directory with no log.jsonl is not a bus.
#
# Every write surface must refuse a root that is not a bus, with the same
# message and a non-zero exit. read is the exception: it quietly answers
# nothing and exits 0, which is written down here as a known so the day it
# grows a refusal this walk goes red.

source "$(dirname "$0")/lib.sh"

step "a directory with no log.jsonl"
ROOT="$SCRATCH/$WALK/nonbus"
rm -rf "$ROOT"
mkdir -p "$ROOT"

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

step "read is not refused — and that is a defect written down"
out=$("$FA" bus read -r "$ROOT" a 2>&1); rc=$?
assert_known "read exits 0 on a non-bus" "0" "$rc" \
  "runRead treats a missing log as an empty one instead of refusing; the refusal shape exists for say, inbox and status but was never wired into read"
assert_eq "and prints nothing" "0" "$(printf '%s' "$out" | sed '/^$/d' | wc -l | tr -d ' ')"

finish
