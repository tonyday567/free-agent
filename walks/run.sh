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
  if [ "$all" -eq 0 ] && { [ "$libdirty" -eq 1 ] || dirty "$walk"; }; then
    printf '\n######## %s  (work in progress, not run)\n' "$name"
    skipped=$((skipped + 1))
    continue
  fi
  printf '\n######## %s\n' "$name"
  if bash "$walk"; then
    passed=$((passed + 1))
  else
    failed=$((failed + 1))
  fi
done

if [ "$skipped" -gt 0 ]; then
  printf '\n######## walks: %d passed, %d failed, %d skipped as work in progress (--all runs them)\n' \
    "$passed" "$failed" "$skipped"
else
  printf '\n######## walks: %d passed, %d failed\n' "$passed" "$failed"
fi
[ "$failed" -eq 0 ] || exit 1
