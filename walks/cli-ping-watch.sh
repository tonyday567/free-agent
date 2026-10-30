#!/usr/bin/env bash
# cli-ping-watch — the wake channel: one file, one change, one exit.
#
# ping-watch touches the ping file if it is missing, then blocks until the
# file changes and prints the new content. A seat that parked itself on
# this primitive should not wake before the ping, and should exit promptly
# after it.

source "$(dirname "$0")/lib.sh"

step "a watcher that blocks until the file changes"
new_root pingw
out="$SCRATCH/$WATCH/pingw.out"
: >"$out"
"$FA" bus ping-watch -r "$ROOT" watcher >"$out" 2>&1 &
wpid=$!
sleep 1
assert_eq "still waiting before the ping" \
  "alive" "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"

step "one change wakes it"
printf 'wake 42' >"$ROOT/.ping-watcher"
i=0
while kill -0 "$wpid" 2>/dev/null && [ "$i" -lt 40 ]; do
  sleep 0.25
  i=$((i + 1))
done
assert_eq "exited after the ping" "gone" "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"
wait "$wpid" 2>/dev/null || true
assert_eq "printed the file content" "wake 42" "$(cat "$out")"

step "a second watcher on an existing ping file does not fire on its own"
printf 'old ping' >"$ROOT/.ping-watcher"
"$FA" bus ping-watch -r "$ROOT" watcher >"$out" 2>&1 &
wpid=$!
sleep 1
assert_eq "no self-fire on the existing content" \
  "alive" "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"
printf 'new ping' >"$ROOT/.ping-watcher"
i=0
while kill -0 "$wpid" 2>/dev/null && [ "$i" -lt 40 ]; do
  sleep 0.25
  i=$((i + 1))
done
assert_eq "fires on the change" "gone" "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"
wait "$wpid" 2>/dev/null || true
assert_eq "with the new content" "new ping" "$(cat "$out")"

finish
