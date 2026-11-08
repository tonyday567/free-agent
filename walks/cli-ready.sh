#!/usr/bin/env bash
# cli-ready — the offer list: what can be taken, what is taken, what is held.
#
# ready is the triage surface: an unclaimed public TASK is offered; a CLAIM
# inside its thread withdraws it; a directed TASK was assigned at creation
# and is never offered; a DONE by the owner completes the thread; a LINK
# blocker holds its target until the blocking task completes.

source "$(dirname "$0")/lib.sh"

step "a public TASK with no claim is offered"
new_root ready
post driver - "TASK · sweep the floor"
assert_eq "one offer" "1" "$(count_lines "$FA" bus ready -r "$ROOT" --depth 1)"
assert_eq "the offer names the task" \
  "1" "$("$FA" bus ready -r "$ROOT" --depth 1 2>/dev/null | grep -c 'sweep the floor' || true)"

step "a CLAIM in the thread withdraws the offer"
printf 'CLAIM · taking' | "$FA" bus post -r "$ROOT" --from rachel --to - --thread 0 >/dev/null
assert_eq "claimed, nothing offered" "0" "$(count_lines "$FA" bus ready -r "$ROOT" --depth 1)"

step "a DONE by the owner completes the thread; a fresh offer returns"
printf 'DONE · swept' | "$FA" bus post -r "$ROOT" --from rachel --to - --thread 0 >/dev/null
post driver - "TASK · mop the floor"
assert_eq "one offer again" "1" "$(count_lines "$FA" bus ready -r "$ROOT" --depth 1)"

step "a directed TASK is assigned at creation, never an offer"
post driver dana "TASK · private thing"
assert_eq "still one offer" "1" "$(count_lines "$FA" bus ready -r "$ROOT" --depth 1)"

step "a blocker LINK holds its target until the blocker completes"
post driver - "TASK · big job"
post driver - "TASK · small job"
post driver - "LINK · 5 blocks 6"
assert_eq "two offers while the block holds" "2" "$(count_lines "$FA" bus ready -r "$ROOT" --depth 1)"
offers=$("$FA" bus ready -r "$ROOT" --depth 1 2>/dev/null)
assert_eq "the blocked task is not among them" \
  "0" "$(printf '%s' "$offers" | grep -c 'small job' || true)"
assert_eq "the blocker is" \
  "1" "$(printf '%s' "$offers" | grep -c 'big job' || true)"
printf 'CLAIM · taking' | "$FA" bus post -r "$ROOT" --from dana --to - --thread 5 >/dev/null
printf 'DONE · big job done' | "$FA" bus post -r "$ROOT" --from dana --to - --thread 5 >/dev/null
assert_eq "the target unblocks when the blocker completes" "2" "$(count_lines "$FA" bus ready -r "$ROOT" --depth 1)"
offers=$("$FA" bus ready -r "$ROOT" --depth 1 2>/dev/null)
assert_eq "and is now offered" \
  "1" "$(printf '%s' "$offers" | grep -c 'small job' || true)"

finish
