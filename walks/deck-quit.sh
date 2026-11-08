#!/usr/bin/env bash
# deck-quit — view q buries; status q kills the buffer and drops its watch/timer.

source "$(dirname "$0")/lib.sh"
source "$(dirname "$0")/deck-lib"

step "a thread view and a live status view"
new_root quit
post alice runner "root"

step "q is exercised through each buffer's actual key binding"
out=$(deck_eval '(progn
  (free-agent-bus-thread 0)
  (let ((buf (current-buffer))
        (key (key-binding (kbd "q"))))
    (call-interactively key)
    (princ (format "thread-key=%S\nthread-alive=%S\nthread-current=%S\nthread-window=%S\n"
                   key (buffer-live-p buf) (eq buf (current-buffer))
                   (and (get-buffer-window buf) t))))
  (free-agent-bus-status)
  (let* ((buf (current-buffer))
         (key (key-binding (kbd "q")))
         (watch free-agent-bus-status--file-watch)
         (timer free-agent-bus-status--periodic-timer)
         (before (list (and watch (file-notify-valid-p watch))
                       (and timer (memq timer timer-list) t))))
    (call-interactively key)
    (princ (format "status-key=%S\nstatus-before=%S\nstatus-alive=%S\nstatus-watch=%S\nstatus-timer=%S"
                   key before (buffer-live-p buf)
                   (and watch (file-notify-valid-p watch))
                   (and timer (memq timer timer-list) t)))))')
assert_eq "thread q buries and status q releases its resources" \
  $'thread-key=quit-window\nthread-alive=t\nthread-current=nil\nthread-window=nil\nstatus-key=kill-current-buffer\nstatus-before=(t t)\nstatus-alive=nil\nstatus-watch=nil\nstatus-timer=nil' "$out"

finish
