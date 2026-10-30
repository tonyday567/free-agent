#!/usr/bin/env bash
# cli-ping-watch — the wake channel: one file, one change, one exit.
#
# ping-watch touches the ping file if it is missing, then blocks until the
# file changes and prints the new content. A seat that parked itself on
# this primitive should not wake before the ping, and should exit promptly
# after it.

source "$(dirname "$0")/lib.sh"

step "a root with no ping file yet"
new_root pingw
out="$ROOT/pingw.out"
: >"$out"

step "a ping file older than the watch's registration window"
printf 'old ping' >"$ROOT/.ping-watcher"
sleep 2
"$FA" bus ping-watch -r "$ROOT" watcher >"$out" 2>&1 &
wpid=$!
sleep 1
assert_eq "waiting on the old content, not fired by it" \
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

step "a missing ping file — the watcher wakes on its own creation"
new_root pingw-cold
out="$ROOT/pingw.out"
: >"$out"
"$FA" bus ping-watch -r "$ROOT" watcher >"$out" 2>&1 &
wpid=$!
sleep 1
assert_known "gone within a second with no external write" "gone" \
  "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)" \
  "runPingWatch touches a missing ping file before the watch registers; the creation event lands in the fresh FSEvents window and fires the handler, so the watcher exits on its own setup instead of waiting — a seat that ping-watches cold wakes instantly on nothing"
wait "$wpid" 2>/dev/null || true
assert_known "and prints nothing" "" "$(cat "$out")" \
  "the self-fire reads the just-created empty file, so the wake channel delivers an empty line — the cold-start ping carries no content either"

finish
