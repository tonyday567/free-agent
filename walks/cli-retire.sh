#!/usr/bin/env bash
# cli-retire — retirement changes circle membership, not the evidence.
#
# retire writes the dead marker without signalling a process. The next status
# shows the name in black, with its directed unread and last post intact; the
# cursor, log, and transcripts remain available for inspection and recovery.

source "$(dirname "$0")/lib.sh"

step "a root with a seat that has been around"
new_root retire
post alice bob "for bob"
post alice carol "for carol, who is alive"
printf 'idle %s %s' "$(date -u +%Y-%m-%dT%H:%M:%S.000000Z)" "$$" >"$ROOT/.hb-carol"
$FA bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "bob's cursor is set before retiring" "0" "$(cat "$ROOT/.cursor-bob")"

step "retire a seat with no heartbeat, then one that was running"
$FA bus retire -r "$ROOT" nobody >/dev/null
assert_eq "no heartbeat before, two fields after" "2" "$(awk '{print NF}' "$ROOT/.hb-nobody")"

step "a seat that was running retires to dead, and keeps its last known pid"
printf 'working 2026-10-31T00:00:00.000000Z 4242' >"$ROOT/.hb-bob"
out=$("$FA" bus retire -r "$ROOT" bob 2>&1)
assert_eq "retire names the seat and the root" "⚫ bob retired on $ROOT" "$out"
assert_eq "and exits 0" "0" "$?"
assert_eq "the heartbeat now reads dead" "dead" "$(awk '{print $1}' "$ROOT/.hb-bob")"
assert_eq "with the pid it last reported, since no process is signalled" "4242" "$(awk '{print $3}' "$ROOT/.hb-bob")"

step "the seat joins the black circle, and the evidence does not move"
status=$("$FA" bus status -r "$ROOT" 2>&1)
assert_eq "bob remains visible as retired with unread and last post" \
  "1" "$(printf '%s' "$status" | grep -c '^⚫ bob (1) post \[#0\]$' || true)"
assert_eq "nobody also has a retired row" \
  "1" "$(printf '%s' "$status" | grep -c '^⚫ nobody (0) no posts$' || true)"
assert_eq "carol remains ready with her own unread mail" \
  "1" "$(printf '%s' "$status" | grep -c '^🟢 carol (1) post \[#1\]$' || true)"
assert_eq "the log is untouched" "2" "$(count_lines cat "$ROOT/log.jsonl")"
assert_eq "bob's cursor survives" "0" "$(cat "$ROOT/.cursor-bob")"
assert_eq "bob's mail is still addressed to bob" "0" "$(ids_of_to bob)"

finish
