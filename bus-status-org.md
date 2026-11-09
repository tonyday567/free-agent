---
description: One Haskell status snapshot and Org census for fab
status: 🟢 done · 9 Nov 2026 · unified census verified and operational in shared Doom
bus:
  root: ~/net
agents:
  - kimi
artifacts:
  - ~/haskell/free-agent
  - ~/.config/doom/config.el
---

## intent

Make `free-agent bus status` one read of the bus: who is present, which circle each name belongs to, how much directed mail is unread, and the last post. `fab-open-status` renders that snapshot as ordinary Org. The API follows this product shape; separate live and cursor-summary projections are retired.

## channel

The Org form is a census header, circle groups, and sparse seat rows:

```org
* 🟢 1 🔴 4 ⚫ 13 | last [#766]
** 🟢
*** free3 (0) post [#766]
** ⚫
*** banjo (0) post [#15]
```

The header includes only non-zero circles, with bare membership counts and the global last-post anchor. A seat row carries its name, directed unread `(N)`, and the last post sent by or addressed to that name; `no posts` is explicit. Rows do not repeat state prose or expose raw cursor positions.

The circle vocabulary is 🟣 working, 🟢 ready, 🟡 parked, 🔴 crashed, ⚫ retired, ⚪ non-fab. Explicit `dead` markers are black and remain visible. A dead pid without that marker is red. No heartbeat is white; a stale heartbeat without verifiable process evidence is also white. Green wake/idle seats become yellow after an hour.

### one API

`free-agent bus status [--depth 0|1|2] [--org]` reads heartbeats, pids, cursors, attached-human markers, and the log once. Plain text and Org are renderers of the same snapshot. The top-level `free-agent status` alias uses the same model. `bus live` and `status --live` are removed.

Depth 0 is the census alone, depth 1 adds seat rows, and depth 2 adds heartbeat, cursor, and last-post evidence. `bus cursor` and `bus inbox` remain the focused diagnostic commands.

Unread uses the inbox rule: posts addressed to the name at or after its cursor. A missing cursor means all addressed history is unread. Rendering never advances a cursor or rewrites the log. Channel and post selectors remain outside this slice.

### fab and legacy retirement

`fab-open-status` calls only `bus status --org`, inserts the result into `*fab-status*`, and anchorizes every `[#N]`. RET/mouse navigation displays the selected thread with the bus root preserved; tooltips carry the first 60 body characters. `SPC y s` invokes this view.

The earlier legacy Elisp, `status --live`, sequential live/status concatenation, cursor summary, repeated idle/retired prose, and raw main-row cursor anchors are not carried forward. No status watcher, timer, Hermes usage collector, or assignment-label scan is introduced.

### ground

The implementation belongs to `src/Free/Agent/Bus/Cli.hs`, the top-level alias in `app/free-agent.hs`, and `fab.el`. Oracles are in `app/Axioma/Bus.hs` and the status/cursor walks; independent Emacs witnesses are `~/.hermes/cache/scratch/fab-status-test.el` and `fab-status-independent.el`.

Observed: `fare gate --docspec` passes, the bus axioma passes, and all 13 walk files pass. Focused results are status 11/11, retirement 12/12, and cursor/inbox 13/13. Both conversation and both status Emacs witnesses pass. The installed executable and shared Doom show `* 🟡 14 ⚫ 13 ⚪ 3 | last [#782]`; Banjo and Fret4 are black, Kimi/Max/Runner are white, and the global anchor's exact 60-character excerpt opens `*fab 782*` with the root preserved. `*fab-status*` remains visible.

## next

No status implementation remains. Use the view and let concrete use determine any further slice. No commit or push is armed.
