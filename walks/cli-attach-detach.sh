#!/usr/bin/env bash
# cli-attach-detach — the human marker: who is reading as a person.
#
# attach stamps a .human-NAME marker — one line of JSON with root, name,
# attached_at and the reader's cursor — and detach removes the marker
# while the cursor and the mail stay as evidence. The runner's runner row
# and the 🟡 human banner in the deck read this file.

source "$(dirname "$0")/lib.sh"

step "attach writes a marker with four fields"
new_root attach
say a runner "mail for the runner"
"$FA" bus cursor set -r "$ROOT" runner 1 >/dev/null
out=$("$FA" bus attach -r "$ROOT" runner 2>&1)
assert_eq "the reply names the marker" \
  "1" "$(printf '%s' "$out" | grep -c "^✓ attached as runner on $ROOT" || true)"
assert_eq "the marker file exists" "present" "$([ -f "$ROOT/.human-runner" ] && echo present || echo absent)"
actual=$(python3 - "$ROOT/.human-runner" <<'PY'
import json, sys
h = json.load(open(sys.argv[1]))
keys = sorted(h.keys())
print(",".join(keys))
print(h["root"])
print(h["name"])
print(h["cursor"])
PY
)
assert_eq "the marker carries exactly root, name, attached_at, cursor" \
  "attached_at,cursor,name,root" "$(printf '%s' "$actual" | sed -n 1p)"
assert_eq "and the root field is this root" "$ROOT" "$(printf '%s' "$actual" | sed -n 2p)"
assert_eq "and the cursor is the cursor file's value at attach time" \
  "1" "$(printf '%s' "$actual" | sed -n 4p)"

step "detach removes the marker, the evidence stays"
out=$("$FA" bus detach -r "$ROOT" runner 2>&1)
assert_eq "detach names what it removed" \
  "1" "$(printf '%s' "$out" | grep -c '^✓ detached as runner on ' || true)"
assert_eq "the marker is gone" "absent" "$([ -f "$ROOT/.human-runner" ] && echo present || echo absent)"
assert_eq "the cursor file survives" "1" "$(cat "$ROOT/.cursor-runner")"
assert_eq "the mail survives" "0" "$(ids_of_to runner)"
out=$("$FA" bus detach -r "$ROOT" runner 2>&1)
assert_eq "a second detach is honest about there being nothing" \
  "1" "$(printf '%s' "$out" | grep -c '^✓ not attached as runner on ' || true)"

finish
