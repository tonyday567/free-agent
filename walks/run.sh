#!/usr/bin/env bash
# walks/run.sh — run the walks and summarise.
#
#   walks/run.sh              the committed suite: walks whose file, and lib.sh itself,
#                             are clean against HEAD. A dirty walk is listed and not
#                             run, because a red tally should mean the handed-in state
#                             failed rather than that an author is mid-edit.
#   walks/run.sh --all        everything, dirty files included
#   walks/run.sh cli-inbox-*  the ones whose name matches
#
# Each walk sets up its own per-run scratch root under $SCRATCH (disposable, and never
# the runner's live root or daemon), asserts expected against actual, and exits
# non-zero on failure. This script aggregates the exit codes.

set -u
here=$(cd "$(dirname "$0")" && pwd)
pattern='*.sh'
all=0
for arg in "$@"; do
  case "$arg" in
  --all) all=1 ;;
  *) pattern=$arg ;;
  esac
done

dirty() { [ -n "$(git -C "$here" status --porcelain -- "$1" 2>/dev/null)" ]; }

# A walk that hangs blocks the whole suite, so each one gets a wall-clock bound. The
# bound is a failure, not a skip: measured 31 Oct when a ping-watch walk died mid-run
# and took the tally with it. Override with WALK_TIMEOUT seconds.
#
# GNU timeout also gets --kill-after, so a walk that traps SIGTERM cannot outlive its
# bound; either way the process group goes with it, so no stray child is left on a live
# root. Where neither gtimeout nor timeout is on PATH the bound cannot be enforced, and
# the run says so once rather than pretending the guarantee holds.
walk_timeout=${WALK_TIMEOUT:-120}
timeout_bin=""
if command -v gtimeout >/dev/null 2>&1; then
  timeout_bin=$(command -v gtimeout)
elif command -v timeout >/dev/null 2>&1; then
  timeout_bin=$(command -v timeout)
fi
kill_after=()
if [ -n "$timeout_bin" ] && "$timeout_bin" --version 2>/dev/null | grep -qi gnu; then
  kill_after=(--kill-after=5)
fi
if [ -z "$timeout_bin" ]; then
  printf '######## note: no timeout(1) on PATH, so walks run without a wall-clock bound\n'
fi
run_walk() {
  if [ -n "$timeout_bin" ]; then
    "$timeout_bin" "${kill_after[@]+"${kill_after[@]}"}" "$walk_timeout" bash "$1"
  else
    bash "$1"
  fi
}

# What kind of uncommitted a file is, for the label: a modified walk is work in
# progress, an untracked one is a walk that does not exist for anyone but its author.
uncommitted_kind() {
  case "$(git -C "$here" status --porcelain -- "$1" 2>/dev/null | awk '{print $1}')" in
  '??') printf 'new, not committed' ;;
  '') printf '' ;;
  *) printf 'work in progress' ;;
  esac
}

libdirty=0
dirty "$here/lib.sh" && libdirty=1

failed=0
passed=0
skipped=0

for walk in "$here"/$pattern; do
  name=$(basename "$walk")
  case "$name" in
  run.sh | lib.sh) continue ;;
  esac
  if [ "$all" -eq 0 ] && [ "$libdirty" -eq 1 ]; then
    printf '\n######## %s  (skipped: lib.sh is uncommitted, so every walk would run against uncommitted helpers)\n' "$name"
    skipped=$((skipped + 1))
    continue
  fi
  if [ "$all" -eq 0 ] && [ -n "$(uncommitted_kind "$walk")" ]; then
    printf '\n######## %s  (%s, not run; --all runs it)\n' "$name" "$(uncommitted_kind "$walk")"
    skipped=$((skipped + 1))
    continue
  fi
  printf '\n######## %s\n' "$name"
  if run_walk "$walk"; then
    passed=$((passed + 1))
  else
    rc=$?
    if [ "$rc" -eq 124 ]; then
      printf '  FAIL   the walk hung and was killed after %ss (WALK_TIMEOUT)\n' "$walk_timeout"
    fi
    failed=$((failed + 1))
  fi
done

ran=$((passed + failed))
if [ "$ran" -eq 0 ]; then
  printf '\n######## walks: nothing ran — %d skipped as dirty or new. --all runs them.\n' "$skipped"
  exit 2
fi

if [ "$skipped" -gt 0 ]; then
  printf '\n######## walks: %d passed, %d failed, %d skipped as dirty or new (--all runs them)\n' \
    "$passed" "$failed" "$skipped"
else
  printf '\n######## walks: %d passed, %d failed\n' "$passed" "$failed"
fi
[ "$failed" -eq 0 ] || exit 1
