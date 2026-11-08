#!/usr/bin/env bash
# sim-conversation — the walks 2.0 witness: scripted participants drive the real
# conversation view end to end, on a scratch root, headless.
#
# One scenario, the span runner-ux proved by hand:
#
#   a scripted runner posts → a seat replies through the real CLI → the view
#   opens and its links resolve → the draft inherits thread and recipients →
#   a live reply lands and the view refreshes under its own timer without
#   losing the draft → the draft submits as an ordinary bus post.
#
# The participants are separate processes posting through the installed CLI
# (`free-agent bus post`), exactly as a seat's turn would; the view under test
# is the real bus-conversation.el, loaded into its own uniquely-named scratch
# daemon (`emacs -Q --daemon`), driven headless over emacsclient, and killed
# and asserted gone at the end. Never the runner's daemon, frames, or root —
# scratch only, under $SCRATCH.
#
# This is a witness, not the harness: one scenario, one file, proving the
# shape ahead of the runner's read (coffee: loom/walks-reimagined.md). It is
# untracked on purpose; the committed suite does not run it, and run.sh
# labels it `new, not committed`.
#
#   bash walks/sim-conversation.sh
#   CONV=/path/to/bus-conversation.el   the view under test (default: scratch)
#   FA=/path/to/free-agent              the binary under test (default: PATH)

source "$(dirname "$0")/lib.sh"

CONV=${CONV:-"$HOME/.hermes/cache/scratch/bus-conversation.el"}
EMACS=${EMACS:-emacs}
DAEMON="sim-conversation.$$"

step "the view under test, and the scratch root"
if [ ! -f "$CONV" ]; then
  printf '  FAIL   no view at %s — set CONV=/path/to/bus-conversation.el\n' "$CONV"
  exit 1
fi
new_root conversation
mkdir -p "$ROOT/loom" "$ROOT/forms"
OBS="$ROOT/obs"
FORMS="$ROOT/forms"

# the card the scenario cites — scratch, so nothing touches coffee/
cat >"$ROOT/loom/walks-reimagined.md" <<'MD'
# walks-reimagined

### the harness
MD
note "view    $CONV"
note "scratch $ROOT"

# sim_form NAME — write one elisp form; $ROOT, $OBS, $CONV and $FA_DIR expand
# here, once. The content is wrapped in a single progn: emacsclient -e evals
# one top-level form and stops, so several forms in a row would silently drop
# everything after the first. Forms write their observation with sim-obs;
# emacsclient's own return value is prin1-quoted and not worth parsing.
sim_form() {
  printf '(progn\n' >"$FORMS/$1.el"
  cat >>"$FORMS/$1.el"
  printf '\n)\n' >>"$FORMS/$1.el"
}

# sim_drive NAME — evaluate that form in the scratch daemon; return $OBS.
sim_drive() {
  rm -f "$OBS"
  local rc=0
  emacsclient -s "$DAEMON" -e "$(cat "$FORMS/$1.el")" >/dev/null 2>&1 || rc=$?
  if [ -f "$OBS" ]; then
    cat "$OBS"
  else
    printf 'NO-OBS rc=%s\n' "$rc"
  fi
}

# the daemon dies with the run, on every path
sim_stop() { emacsclient -s "$DAEMON" -e '(kill-emacs)' >/dev/null 2>&1 || true; }
trap sim_stop EXIT

step "the scripted runner posts through the real CLI"
post runner egret "TASK reply with the links: [walks-reimagined.md] and [#0]"
assert_eq "post 0: runner → egret, no parents" \
  '["egret"] []' "$(field 0 to) $(field 0 thread)"

step "the seat actor replies through the real CLI"
printf 'reply: [egret] [walks-reimagined.md] [#0]' |
  "$FA" bus post -r "$ROOT" --from egret --to runner --thread 0 >/dev/null
assert_eq "post 1: egret → runner on thread 0" \
  '"egret" [0]' "$(field 1 from) $(field 1 thread)"

step "scripted seat state: a cursor and a heartbeat, written as the bus writes them"
"$FA" bus cursor set -r "$ROOT" egret 0 >/dev/null
printf 'idle %s %s\n' "$(date -u +%Y-%m-%dT%H:%M:%S.000000Z)" "$$" >"$ROOT/.hb-egret"
assert_eq "the cursor reads back through the CLI" "0" "$("$FA" bus cursor get -r "$ROOT" egret)"

step "a uniquely-named scratch daemon loads the view, headless"
FA_DIR="$(cd "$(dirname "$(command -v "$FA")")" && pwd)"
PATH="$FA_DIR:$PATH" "$EMACS" -Q --daemon="$DAEMON" >/dev/null 2>&1
ready=""
for _ in $(seq 1 60); do
  if emacsclient -s "$DAEMON" -e 't' >/dev/null 2>&1; then ready=1; break; fi
  sleep 0.2
done
if [ -z "$ready" ]; then
  bad "the daemon answers" "ready" "no answer after 12s"
  finish
fi
ok "the daemon answers; name $DAEMON — the runner's server is untouched"

step "arm the view in the daemon: scratch root, a fast refresh dial"
sim_form prelude <<EOF
(load-file "$CONV")
(setq exec-path (cons "$FA_DIR" exec-path))
(setq bus-conversation-root "$ROOT"
      bus-conversation-coffee-root "$ROOT"
      bus-conversation-refresh-interval 0.3)
(defvar sim-ws nil)
(defun sim-obs (text) (with-temp-file "$OBS" (insert text)))
(sim-obs "prelude-ok")
EOF
assert_eq "the view loads; sim-obs writes through" "prelude-ok" "$(sim_drive prelude)"

step "the view opens on the scratch root — live, focused at the latest post"
sim_form open <<EOF
(bus-conversation-open-log)
(sim-obs (format "posts=%s\nfocus=%s\nthread=%s\nto=%s"
                 (mapconcat (lambda (p) (number-to-string (bus-conversation--id p)))
                            bus-conversation--posts ",")
                 bus-conversation--focus
                 bus-conversation--draft-thread
                 (string-join bus-conversation--draft-recipients ",")))
EOF
assert_eq "both posts, focus at the latest, draft aimed by inheritance" \
  $'posts=0,1\nfocus=1\nthread=1\nto=egret' "$(sim_drive open)"

step "the rendering: runner decoration, both bodies, the inherited draft line"
sim_form render <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(sim-obs
 (format "decorated=%s\nreply=%s\ndraft=%s"
         (and (string-match-p (regexp-quote "⬡ TASK reply with the links") (buffer-string)) t)
         (and (string-match-p (regexp-quote "reply: [egret] [walks-reimagined.md] [#0]")
                              (buffer-string)) t)
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward "^--- draft .*$" nil t)
             (match-string 0)))))
EOF
assert_eq "the ⬡ derives from from:runner; the draft line names its aim" \
  $'decorated=t\nreply=t\ndraft=--- draft · thread #1 · to egret ---' "$(sim_drive render)"

step "the [#0] link refocuses the post, its conversation intact"
sim_form link-post <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(goto-char (point-min))
(search-forward "[#0]" nil t)
(button-activate (button-at (match-beginning 0)))
(sim-obs (format "focus=%s posts=%s"
                 bus-conversation--focus
                 (mapconcat (lambda (p) (number-to-string (bus-conversation--id p)))
                            bus-conversation--posts ",")))
EOF
assert_eq "post link opens post 0 with both posts in view" \
  $'focus=0 posts=0,1' "$(sim_drive link-post)"

step "the card link opens the scratch card"
sim_form link-card <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(goto-char (point-min))
(search-forward "[walks-reimagined.md]" nil t)
(button-activate (button-at (match-beginning 0)))
(sim-obs (format "card=%s" (buffer-file-name)))
EOF
assert_eq "card link resolves under the scratch coffee root" \
  "card=$ROOT/loom/walks-reimagined.md" "$(sim_drive link-card)"

step "the seat link opens runner ↔ egret with live status"
sim_form link-seat <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(goto-char (point-min))
(search-forward "[egret]" nil t)
(button-activate (button-at (match-beginning 0)))
(sim-obs (format "seat=%s\nposts=%s\nheader=%s\nstatus=%s\ncursor=%s\nthread=%s\nto=%s"
                 bus-conversation--seat
                 (mapconcat (lambda (p) (number-to-string (bus-conversation--id p)))
                            bus-conversation--posts ",")
                 (and (string-match-p (regexp-quote "runner ↔ egret · ") (buffer-string)) t)
                 (and (string-match-p "egret — idle [0-9]+s ago" (buffer-string)) t)
                 (and (string-match-p (regexp-quote "cursor 0") (buffer-string)) t)
                 bus-conversation--draft-thread
                 (string-join bus-conversation--draft-recipients ",")))
EOF
assert_eq "seat view: both exchanges; status read from the scripted heartbeat" \
  $'seat=egret\nposts=0,1\nheader=t\nstatus=t\ncursor=t\nthread=1\nto=egret' "$(sim_drive link-seat)"

step "the draft inherits; a live reply lands under the view's own timer"
sim_form draft <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(bus-conversation-open-log)
(goto-char bus-conversation--draft-start)
(insert "proceed with parcel two")
(goto-char (+ bus-conversation--draft-start 5))
(setq sim-ws (window-start))
(bus-conversation--stop-timer)
(sim-obs (format "thread=%s\nto=%s\noffset=%s\ndraft=%s"
                 bus-conversation--draft-thread
                 (string-join bus-conversation--draft-recipients ",")
                 (- (point) bus-conversation--draft-start)
                 (buffer-substring-no-properties bus-conversation--draft-start (point-max))))
EOF
assert_eq "a draft typed on the inherited aim; timer paused so the pause is provable" \
  $'thread=1\nto=egret\noffset=5\ndraft=proceed with parcel two' "$(sim_drive draft)"

printf '🟢 [egret] streaming — second response while drafting' |
  "$FA" bus post -r "$ROOT" --from egret --to runner --thread 1 >/dev/null
assert_eq "post 2 lands in the log while the draft is open" '[1]' "$(field 2 thread)"

sim_form paused <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(sim-obs (format "in-view=%s"
                 (and (string-match-p (regexp-quote "second response while drafting")
                                      (buffer-string)) t)))
EOF
assert_eq "paused, the view has not moved — the refresh is the timer's to make" \
  "in-view=nil" "$(sim_drive paused)"

sim_form arm <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(bus-conversation--start-timer)
(sim-obs (format "armed=%s" (and (timerp bus-conversation--timer) t)))
EOF
assert_eq "timer re-armed at the scenario's dial" "armed=t" "$(sim_drive arm)"

sleep 1.2
sim_form landed <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(sim-obs (format "in-view=%s\ndraft=%s\noffset=%s\nthread=%s\nto=%s\nwindow-start=%s\nposts=%s"
                 (and (string-match-p (regexp-quote "second response while drafting")
                                      (buffer-string)) t)
                 (buffer-substring-no-properties bus-conversation--draft-start (point-max))
                 (- (point) bus-conversation--draft-start)
                 bus-conversation--draft-thread
                 (string-join bus-conversation--draft-recipients ",")
                 (equal (window-start) sim-ws)
                 (mapconcat (lambda (p) (number-to-string (bus-conversation--id p)))
                            bus-conversation--posts ",")))
EOF
assert_eq "the new post arrived; draft, offset, aim and window all survived" \
  $'in-view=t\ndraft=proceed with parcel two\noffset=5\nthread=1\nto=egret\nwindow-start=t\nposts=0,1,2' \
  "$(sim_drive landed)"

step "the draft submits as an ordinary bus post — a real CLI write from the view"
sim_form submit <<EOF
(set-buffer (get-buffer "*bus conversation*"))
(bus-conversation-submit-draft)
(sim-obs (format "draft-now=%s\nposts=%s\nin-view=%s"
                 (buffer-substring-no-properties bus-conversation--draft-start (point-max))
                 (mapconcat (lambda (p) (number-to-string (bus-conversation--id p)))
                            bus-conversation--posts ",")
                 (and (string-match-p (regexp-quote "proceed with parcel two")
                                      (buffer-substring-no-properties (point-min)
                                                                      bus-conversation--draft-start))
                      t)))
EOF
assert_eq "the draft left the view; the submitted post joined it" \
  $'draft-now=\nposts=0,1,2,3\nin-view=t' "$(sim_drive submit)"
assert_eq "the log carries it: runner → egret, threaded to the reply" \
  '"runner" ["egret"] [1]' "$(field 3 from) $(field 3 to) $(field 3 thread)"
assert_eq "the body is the draft, trimmed" '"proceed with parcel two"' "$(field 3 body)"

step "the daemon dies with the scenario"
emacsclient -s "$DAEMON" -e '(kill-emacs)' >/dev/null 2>&1 || true
gone=""
for _ in $(seq 1 25); do
  if ! emacsclient -s "$DAEMON" -e 't' >/dev/null 2>&1; then gone=1; break; fi
  sleep 0.2
done
assert_eq "no daemon survives the run" "1" "$gone"

finish
