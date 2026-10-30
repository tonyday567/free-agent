#!/usr/bin/env bash
# Intent: real GUI and terminal frames with Evil, and nothing left behind.
# Walk: a unique scratch daemon gets one GUI and one terminal frame. Both show
# the three-window deck with focus in status. <tab> and q work under Evil in
# both. Terminal TAB/C-i goes to evil-jump-forward instead of the deck (known).
# Boundary: installed Evil is loaded, not Doom's init. Key bindings are
# resolved in each real frame; no OS keyboard events are injected.
source "$(cd "$(dirname "$0")" && pwd)/lib.sh"
source "$(cd "$(dirname "$0")" && pwd)/deck-lib"

new_root frame
: >"$ROOT/log.jsonl"
printf 'root\n' | free-agent bus say -r "$ROOT" --from alice --to runner >/dev/null

server="deck-frame-$$-$RANDOM"
daemon_pid=
terminal_launcher=
terminal_pid=

stop_scratch() {
  emacsclient -s "$server" -e '(kill-emacs)' >/dev/null 2>&1 || true
  local i
  for ((i = 0; i < 50; i++)); do
    [[ -z $daemon_pid ]] || ! kill -0 "$daemon_pid" 2>/dev/null && break
    sleep 0.1
  done
  if [[ -n $daemon_pid ]] && kill -0 "$daemon_pid" 2>/dev/null; then
    kill "$daemon_pid" 2>/dev/null || true
  fi
  if [[ -n $terminal_pid ]] && kill -0 "$terminal_pid" 2>/dev/null; then
    kill "$terminal_pid" 2>/dev/null || true
  fi
  if [[ -n $terminal_launcher ]] && kill -0 "$terminal_launcher" 2>/dev/null; then
    kill "$terminal_launcher" 2>/dev/null || true
  fi
  if [[ -n $terminal_launcher ]]; then
    wait "$terminal_launcher" 2>/dev/null || true
  fi
}
trap stop_scratch EXIT

step "start a scratch daemon with installed Evil and no Doom init"
evil_dir=
for candidate in "$HOME"/.config/emacs/.local/straight/build-*/evil; do
  [[ -f $candidate/evil.el ]] && evil_dir=$candidate
done
if [[ -z $evil_dir ]]; then
  echo "  FAIL   installed Evil not found; configured key delivery is out of scope"
  exit 1
fi
cat >"$ROOT/boot.el" <<EOF
(add-to-list 'load-path "$evil_dir")
(require 'evil)
(load "$DECK")
(evil-mode 1)
(setq free-agent-bus-root "$ROOT"
      free-agent-bus-name "runner"
      free-agent-bus-status-refresh-interval 3600.0
      free-agent-bus-auto-refresh-interval 3600.0)
EOF
emacs -Q --daemon="$server" -l "$ROOT/boot.el" >"$ROOT/daemon.log" 2>&1
for ((i = 0; i < 100; i++)); do
  daemon_pid=$(emacsclient -s "$server" -e '(emacs-pid)' 2>/dev/null) && break
  sleep 0.1
done
if [[ ! $daemon_pid =~ ^[0-9]+$ ]]; then
  echo "  FAIL   scratch daemon did not start"
  cat "$ROOT/daemon.log"
  exit 1
fi

gui_type=$(emacsclient -s "$server" -n -c -e '(framep (selected-frame))')
assert_eq "GUI scratch frame" "ns" "$gui_type"

step "open a terminal frame on a pseudo-terminal"
python3 - "$server" "$ROOT" <<'PY' &
import os
import pty
import select
import sys

server, root = sys.argv[1:]
pid, fd = pty.fork()
if pid == 0:
    os.execvp("emacsclient", ["emacsclient", "-s", server, "-t"])
with open(os.path.join(root, "terminal.pid"), "w") as f:
    f.write(str(pid))
with open(os.path.join(root, "terminal.out"), "wb", buffering=0) as out:
    while True:
        try:
            ready, _, _ = select.select([fd], [], [], 0.2)
            if ready:
                data = os.read(fd, 4096)
                if not data:
                    break
                out.write(data)
            done, _ = os.waitpid(pid, os.WNOHANG)
            if done:
                break
        except (OSError, ChildProcessError):
            break
PY
terminal_launcher=$!
terminal_present=
for ((i = 0; i < 100; i++)); do
  terminal_present=$(emacsclient -s "$server" -e '(catch (quote found) (dolist (f (frame-list)) (when (eq (framep f) t) (throw (quote found) t))) nil)' 2>/dev/null)
  [[ $terminal_present == t ]] && break
  sleep 0.1
done
assert_eq "terminal scratch frame" "t" "$terminal_present"
if [[ $terminal_present != t ]]; then
  cat "$ROOT/terminal.out"
  exit 1
fi
terminal_pid=$(cat "$ROOT/terminal.pid")

step "probe deck windows, focus, and Evil keymaps in both frames"
cat >"$ROOT/probe.el" <<'EOF'
(defun deck-walk-frame-probe (kind)
  (let ((frame
         (catch 'found
           (dolist (f (frame-list))
             (when (eq (framep f) kind) (throw 'found f)))
           nil)))
    (with-selected-frame frame
      (free-agent-bus-deck)
      (let ((window-count (length (window-list)))
            (selected (buffer-name (window-buffer (selected-window)))))
        (with-current-buffer free-agent-bus-log-buffer-name
          (evil-local-mode 1)
          (evil-normal-state)
          (let ((result
                 (format "%s|%d|%s|%s|%s|%s|%s|%s|%s"
                         (framep frame) window-count selected
                         major-mode evil-local-mode evil-state
                         (key-binding (kbd "TAB"))
                         (key-binding (kbd "<tab>"))
                         (key-binding (kbd "q")))))
            (with-temp-file (expand-file-name (format "probe-%s" kind)
                                              free-agent-bus-root)
              (insert result))))))))
EOF
emacsclient -s "$server" -e "(load \"$ROOT/probe.el\")" >/dev/null
emacsclient -s "$server" -e "(deck-walk-frame-probe 'ns)" >/dev/null
emacsclient -s "$server" -e "(deck-walk-frame-probe 't)" >/dev/null
IFS='|' read -r gui_frame gui_windows gui_focus gui_major gui_evil gui_state gui_tab gui_angle gui_q <"$ROOT/probe-ns"
IFS='|' read -r tty_frame tty_windows tty_focus tty_major tty_evil tty_state tty_tab tty_angle tty_q <"$ROOT/probe-t"
assert_eq "GUI frame probed" "ns" "$gui_frame"
assert_eq "GUI deck has three windows" "3" "$gui_windows"
assert_eq "GUI deck focuses status" "*free-agent-bus-status*" "$gui_focus"
assert_eq "GUI probe uses log mode" "free-agent-bus-log-mode" "$gui_major"
assert_eq "GUI probe enables local Evil" "t" "$gui_evil"
assert_eq "GUI probe uses normal state" "normal" "$gui_state"
assert_eq "GUI <tab> reaches deck under Evil" "free-agent-bus-cycle-depth" "$gui_angle"
assert_eq "GUI TAB/C-i follows Evil" "evil-jump-forward" "$gui_tab"
assert_eq "GUI q reaches deck under Evil" "quit-window" "$gui_q"
assert_eq "terminal frame probed" "t" "$tty_frame"
assert_eq "terminal deck has three windows" "3" "$tty_windows"
assert_eq "terminal deck focuses status" "*free-agent-bus-status*" "$tty_focus"
assert_eq "terminal probe uses log mode" "free-agent-bus-log-mode" "$tty_major"
assert_eq "terminal probe enables local Evil" "t" "$tty_evil"
assert_eq "terminal probe uses normal state" "normal" "$tty_state"
assert_eq "terminal <tab> reaches deck under Evil" "free-agent-bus-cycle-depth" "$tty_angle"
assert_eq "terminal q reaches deck under Evil" "quit-window" "$tty_q"
assert_known "terminal TAB reaches deck under Evil" "free-agent-bus-cycle-depth" "$tty_tab" \
  "Evil's auxiliary map leaves TAB/C-i on evil-jump-forward; <tab> still works"

step "kill the scratch daemon and reap both clients"
stop_scratch
trap - EXIT
daemon_live=no
terminal_live=no
kill -0 "$daemon_pid" 2>/dev/null && daemon_live=yes
kill -0 "$terminal_pid" 2>/dev/null && terminal_live=yes
assert_eq "scratch daemon is gone" "no" "$daemon_live"
assert_eq "terminal client is gone" "no" "$terminal_live"
