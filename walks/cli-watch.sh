#!/usr/bin/env bash
# cli-watch — a watcher skips history, delivers new addressed raw JSON, and dies clean.
#
# watch has no --depth flag: its output contract is the stored JSON frame. Existing
# posts are deliberately skipped, so the walk registers the watcher before the new
# posts rather than expecting the seed post to be replayed.

source "$(dirname "$0")/lib.sh"

step "one old post, then a watcher for yung"
new_root watch
say alice yung "old"
out="$ROOT/watch.out"
: >"$out"
"$FA" bus watch -r "$ROOT" yung >"$out" 2>&1 &
wpid=$!
cleanup() {
  if kill -0 "$wpid" 2>/dev/null; then kill "$wpid" 2>/dev/null; fi
  wait "$wpid" 2>/dev/null || true
}
trap cleanup EXIT
sleep 2
assert_eq "the watcher is waiting" "alive" \
  "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"
assert_eq "the pre-watch post is not replayed" "0" \
  "$(wc -l <"$out" | tr -d ' ')"

step "one new post for yung and one for bob"
say alice yung "new"
say alice bob "other"
i=0
while [ "$(wc -l <"$out" | tr -d ' ')" -lt 1 ] && [ "$i" -lt 40 ]; do
  sleep 0.25
  i=$((i + 1))
done
assert_eq "exactly the addressed new post arrives" "1" \
  "$(json_ids <"$out")"
assert_eq "its body is the new body" "new" \
  "$(python3 -c 'import json,sys; print(",".join(json.loads(line)["body"] for line in sys.stdin if line.strip()))' <"$out")"
assert_eq "no second post leaks through" "1" \
  "$(wc -l <"$out" | tr -d ' ')"
assert_eq "watching creates no delivery cursor" "absent" \
  "$(test -e "$ROOT/.cursor-yung" && echo present || echo absent)"
assert_eq "the watcher keeps waiting after delivery" "alive" \
  "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"

step "killing the watcher leaves no process behind"
kill "$wpid"
wait "$wpid" 2>/dev/null || true
trap - EXIT
assert_eq "the watcher is gone" "gone" \
  "$(kill -0 "$wpid" 2>/dev/null && echo alive || echo gone)"

finish
