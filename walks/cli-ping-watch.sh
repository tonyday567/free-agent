#!/usr/bin/env bash
# cli-ping-watch — the wake channel: one file, one change, one exit.
#
# ping-watch touches the ping file if it is missing, then blocks until the
# file changes and prints the new content. A seat parked on this primitive
# should wake on the change after registration and exit promptly.
#
# The substrate is FSEvents, and two of its behaviours shape the walk. A
# write can be delivered historically once registration completes, and a
# write that lands inside the registration gap can be missed entirely. The
# walk cannot observe registration from outside, so it never asserts an
# ordering it cannot see: the change step arms once, and if the watcher is
# still up after a generous window the first write fell in the gap and a
# second write — certainly post-registration — carries the case. And no
# watcher outlives the walk: the missed-event path used to leak a blocked
# ping-watch per failed run (measured 31 Oct, yin: repetition degraded six
# runs past 420s), so a trap kills the current one on every exit path.

source "$(dirname "$0")/lib.sh"

wpid=""
cleanup() { [ -n "$wpid" ] && kill "$wpid" 2>/dev/null; }
trap cleanup EXIT
trap 'exit 130' TERM INT

# true when $wpid has exited: reaped, or an un-reaped zombie (kill -0 alone
# reports zombies as alive, and bash 3.2 cannot wait with a timeout).
gone() {
  kill -0 "$wpid" 2>/dev/null || return 0
  [ "$(ps -p "$wpid" -o state= 2>/dev/null | tr -d ' ')" = "Z" ]
}

# wait_gone SECS — poll until $wpid exits; success when it did.
wait_gone() {
  local i=0 lim=$(( $1 * 4 ))
  while ! gone && [ "$i" -lt "$lim" ]; do
    sleep 0.25
    i=$((i + 1))
  done
  gone
}

step "a change wakes the watcher and it prints the content"
new_root pingw
out="$ROOT/pingw.out"
printf 'seed' >"$ROOT/.ping-watcher"
: >"$out"
"$FA" bus ping-watch -r "$ROOT" watcher >"$out" 2>&1 &
wpid=$!
printf 'wake 1' >"$ROOT/.ping-watcher"
if wait_gone 15; then
  ok "exited after the change"
  woke=$(cat "$out")
  case "$woke" in
    seed)
      note "woke on the pre-registration content, delivered historically" ;;
    "wake 1")
      ok "printed the file's content at wake" ;;
    *)
      bad "printed the file's content at wake" "seed or wake 1" "$woke" ;;
  esac
else
  # The first write fell inside the registration gap; the second is
  # certainly post-registration.
  printf 'wake 2' >"$ROOT/.ping-watcher"
  if wait_gone 15; then
    ok "exited after the re-armed change"
    assert_eq "printed the file's content at wake" "wake 2" "$(cat "$out")"
  else
    bad "the watcher missed two changes fifteen seconds apart" "gone" "alive"
  fi
fi
wait "$wpid" 2>/dev/null || true
wpid=""

step "a missing ping file — the watcher wakes on its own creation"
new_root pingw-cold
out="$ROOT/pingw.out"
: >"$out"
"$FA" bus ping-watch -r "$ROOT" watcher >"$out" 2>&1 &
wpid=$!
assert_known "gone within fifteen seconds, with no external write" "gone" \
  "$(if wait_gone 15; then echo gone; else echo alive; fi)" \
  "runPingWatch touches a missing ping file before the watch registers; the creation event lands in the fresh FSEvents window and fires the handler, so the watcher exits on its own setup instead of waiting — a seat that ping-watches cold wakes instantly on nothing. The event is historical, so it cannot be missed, only delayed"
wait "$wpid" 2>/dev/null || true
wpid=""
assert_known "and prints nothing" "" "$(cat "$out")" \
  "the self-fire reads the just-created empty file, so the wake channel delivers an empty line — the cold-start ping carries no content either"

finish
