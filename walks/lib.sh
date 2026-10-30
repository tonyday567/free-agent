#!/usr/bin/env bash
# walks/lib.sh — shared helpers for the bus behaviour walks.
#
# A walk sets up its own state, invokes one surface, and prints expected versus
# actual per step. It exits non-zero when a step fails.
#
# Two kinds of assertion, and the difference matters:
#
#   assert_eq   the behaviour we want. A failure is a bug to fix.
#   known       behaviour that is wrong but currently true, written down with its
#               reason so it is checkable. It passes while the behaviour holds and
#               fails the moment it changes — a tripwire, not an excuse.
#
# Scratch is disposable and never the runner's: no live root, no live daemon, no
# frames. Set SCRATCH to move it.

set -u

FA=${FA:-free-agent}
SCRATCH=${SCRATCH:-"$HOME/.hermes/cache/scratch/deck-walks"}
WALK=$(basename "${BASH_SOURCE[1]:-$0}" .sh)
ROOT=""

PASS=0
FAIL=0
KNOWN=0

step() { printf '\n== %s\n' "$*"; }
ok() { printf '  PASS   %s\n' "$1"; PASS=$((PASS + 1)); }
bad() {
  printf '  FAIL   %s\n         expected: %s\n         actual:   %s\n' "$1" "$2" "$3"
  FAIL=$((FAIL + 1))
}
note() { printf '  note   %s\n' "$*"; }

assert_eq() { [ "$2" = "$3" ] && ok "$1" || bad "$1" "$2" "$3"; }

# assert_known NAME EXPECTED ACTUAL REASON — a defect written down as checkable.
assert_known() {
  printf '  KNOWN  %s — %s\n' "$1" "$4"
  KNOWN=$((KNOWN + 1))
  assert_eq "$1 (known)" "$2" "$3"
}

# new_root NAME — a fresh scratch bus with an empty log, which is what the CLI
# accepts: an empty log.jsonl is a bus, a missing one is refused.
#
# The path carries this shell's pid, because seats run the suite concurrently and a
# shared path means two runs clobber each other's roots — measured 31 Oct: a suite
# that passes three times in a row failed once while four walk processes were up.
new_root() {
  ROOT="$SCRATCH/$WALK/$1.$$"
  rm -rf "$ROOT"
  mkdir -p "$ROOT"
  : >"$ROOT/log.jsonl"
  printf '  root   %s\n' "$ROOT"
}

# say FROM TO BODY... — TO is passed verbatim so a walk can test '', which is not
# the same as omitting the flag.
say() {
  local from="$1" to="$2"
  shift 2
  if [ "$to" = "-" ]; then
    printf '%s' "$*" | "$FA" bus say -r "$ROOT" --from "$from" >/dev/null
  else
    printf '%s' "$*" | "$FA" bus say -r "$ROOT" --from "$from" --to "$to" >/dev/null
  fi
}

# field ID KEY — one field of one post, rendered for comparison. Lists render as
# JSON so `[]` (public), `[""]` (a malformed empty recipient) and `["bob"]` differ.
field() {
  python3 - "$ROOT/log.jsonl" "$1" "$2" <<'PY'
import json, sys
path, want, key = sys.argv[1], int(sys.argv[2]), sys.argv[3]
for line in open(path):
    line = line.strip()
    if not line:
        continue
    post = json.loads(line)
    if post.get("id") == want:
        print(json.dumps(post.get(key)))
        break
PY
}

# ids_of_to NAME — the ids addressed to NAME, comma-joined, in log order.
ids_of_to() {
  python3 - "$ROOT/log.jsonl" "$1" <<'PY'
import json, sys
path, name = sys.argv[1], sys.argv[2]
out = []
for line in open(path):
    line = line.strip()
    if line and name in (json.loads(line).get("to") or []):
        out.append(str(json.loads(line)["id"]))
print(",".join(out))
PY
}

# count_lines CMD... — stdout line count of a command, for the compact surfaces.
count_lines() {
  "$@" 2>/dev/null | wc -l | tr -d ' '
}

# json_ids — the ids of JSON post lines on stdin, comma-joined: what a read returns.
json_ids() {
  python3 -c 'import sys,json; print(",".join(str(json.loads(l)["id"]) for l in sys.stdin if l.strip()))'
}

# dot_ids — the leading id of each depth-0 line on stdin, comma-joined: what a
# depth-0 view returns (`0 alice→bob`). Asserting ids rather than a line count
# keeps the assertion honest: a count passes for a header line or the wrong posts.
dot_ids() {
  awk 'NF {print $1}' | paste -sd, -
}

finish() {
  printf '\n-- %s: %d passed, %d failed, %d known\n' "$WALK" "$PASS" "$FAIL" "$KNOWN"
  [ "$FAIL" -eq 0 ] || exit 1
  exit 0
}
