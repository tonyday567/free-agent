#!/usr/bin/env bash
# cli-retire — a seat leaves the live roster, and nothing else is lost.
#
# retire is the honest way to end a seat: it writes the dead marker, so the roster
# stops answering for a process that is gone, while the cursor, the log and the
# transcripts stay as evidence. The runner's complaint on 30 Oct was the opposite
# case — carafe, dripper and tamper showing idle for 22 hours and chemex a
# permanent no-heartbeat row — so this walk pins the good path down.

source "$(dirname "$0")/lib.sh"

step "a root with a seat that has been around"
new_root retire
say alice bob "for bob"
say alice carol "for carol, who is alive"
printf 'alive 2026-10-31T00:00:00.000000Z 4242' >"$ROOT/.hb-carol"
$FA bus cursor set -r "$ROOT" bob 0 >/dev/null
assert_eq "bob's cursor is set before retiring" "0" "$(cat "$ROOT/.cursor-bob")"

step "retire bob"
out=$("$FA" bus retire -r "$ROOT" bob 2>&1)
assert_eq "retire names the seat and the root" "🔴 bob retired on $ROOT" "$out"
assert_eq "and exits 0" "0" "$?"
assert_eq "the heartbeat now reads dead" "dead" "$(awk '{print $1}' "$ROOT/.hb-bob")"
assert_eq "with no pid, since no process is signalled" "2" "$(awk '{print NF}' "$ROOT/.hb-bob")"

step "the seat leaves the live roster, the evidence does not"
live=$("$FA" bus status -r "$ROOT" --live 2>&1)
assert_eq "bob is gone from the live view" "0" "$(printf '%s' "$live" | grep -c 'bob' || true)"
assert_eq "carol, who is alive, is named" "1" "$(printf '%s' "$live" | grep -c 'carol' || true)"
assert_eq "the log is untouched" "2" "$(count_lines cat "$ROOT/log.jsonl")"
assert_eq "bob's cursor survives" "0" "$(cat "$ROOT/.cursor-bob")"
assert_eq "bob's mail is still addressed to bob" "0" "$(ids_of_to bob)"

finish
