#!/usr/bin/env bash
# walks/run.sh — run every walk, in order, and summarise.
#
#   walks/run.sh              every walk
#   walks/run.sh cli-inbox-*  the ones whose name matches
#
# Each walk sets up its own scratch root under $SCRATCH (disposable, and never the
# runner's live root or daemon), asserts expected against actual, and exits
# non-zero on failure. This script aggregates the exit codes.

set -u
here=$(cd "$(dirname "$0")" && pwd)
pattern=${1:-'*.sh'}
failed=0
passed=0

for walk in "$here"/$pattern; do
  name=$(basename "$walk")
  [ "$name" = "run.sh" ] && continue
  [ "$name" = "lib.sh" ] && continue
  printf '\n######## %s\n' "$name"
  if bash "$walk"; then
    passed=$((passed + 1))
  else
    failed=$((failed + 1))
  fi
done

printf '\n######## walks: %d passed, %d failed\n' "$passed" "$failed"
[ "$failed" -eq 0 ] || exit 1
