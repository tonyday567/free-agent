# Revision history for free-agent

## unreleased

- **Seat self-halt (tail-chatter fix).** A seat whose own reply carries a
  🔵 stand-down mark now stops its loop — judged at commit time, since the
  F2 self-post skip means the seat never reads its own mark back. 🟢 stays
  exchange-level: a seat may land one exchange and host more. Fixed in both
  runners: `runSeatBus` stops mid-batch (later posts in the batch go
  unanswered) and `runAgentLoop` returns `Halt` after scribing. Pinned by
  the "bus seat self-halt" oracle in `free-agent-axioma` (red without the
  fix); live-verified on the pit bus — a follow-up post to a stood-down
  seat gets silence. R3c tail-chatter closed.
- **Stale cliHost fake repaired.** The axioma's fake CLI emitted the
  pre-0.18.2 `Session:` line; `parseSessionId` now matches `session_id:`,
  so the fake never persisted a session and the whole axioma crashed on a
  missing file. Fake updated to the current format.

- **Status unification.** Top-level `free-agent status [ROOT]` now delegates to
  the same implementation as `free-agent bus status`. Added `--threshold` / `-t`
  to configure the live/quiet threshold in seconds (default 900). Seats whose
  latest own post is a halt mark (🟢/🔵) are reported as `done`, not `behind`.
- **`tailLog` oracle** in `free-agent-axioma`: offset draining, partial trailing
  line left for the next drain, and `Halt` mid-batch all pinned by assertions.
- **Wakeup paths** documented in `buff/free-agent-bus.md`: when to use `watch`,
  `ping-watch`, or polling.
- **Seat supervision.** Handler exceptions in `runAgentLoop` (file-backed
  seats) and `runSeatBus` (in-process seats) are caught with `try`, logged to
  `stderr`, and converted to a 🔴 escalation post addressed to the original
  sender (and the pitboss, when one is configured). The cursor advances and
  the seat keeps listening. Previously a failing `--cmd` or live agent call
  killed the whole seat.
- **Seats scribe in-process.** `findScribe`/`scribePost` (shell-out to the
  deleted `free-agent-bus` executable) removed; seats and `bus post` use
  `postLocal`, which assigns the id from the file under the exclusive lock —
  coherent under concurrent posters. New `withBus` bracket.
- **`tailLog` is offset-based.** Read handles are closed before callbacks
  run (callbacks may append in-process; GHC locks files per process, so a
  held read handle made the seat's own append fail with "resource busy").
  Partial trailing lines are left for the next drain. Callbacks now return
  `Flow` (`Continue`/`Halt`).
- **Decided quiet in the seats.** Delivered halt marks (🟢/🔵) stop the
  loop — control, not content, so a mark gets silence, not a reply. 🔴 is
  relayed to the pitboss (when `--pitboss` names one) and halts. The
  `--quiesce N` bridge now posts 🔵 "standing down" (was 🟡, which
  collided with the claim mark). Same semantics in `runSeatBus`.
  Behaviour change: seats that previously ran until killed or quiesced now
  also halt on marks addressed to them.
- **Dedupe.** `busDeliversTo` deleted (one `deliversTo`, from
  circuits-agent); CLI cursor helpers deleted (one cursor module,
  `Free.Agent.Bus.File`). `readSince` cursor convention fixed to "next
  unprocessed id" (`>=`), so post id 0 is no longer invisible to a fresh
  seat.
- **Deck.** `free-agent-bus-deck.el` posts via `free-agent bus post
  --root ROOT`; broadcast is `to: ["all"]` (the deck's `to: []` broadcast
  was silently discard under current bus semantics).
- `free-agent-axioma` gains the bus-seat oracle: reply with thread edge,
  coherent ids, silence after the halt mark.

## 0.1.0.0 -- 2026-08-01

* Package role **ABOVE** circuits-agent: free syntax that folds into Agent/Shard.
* `Free.Agent.Syntax` / `Layer` — free category (`Lift`/`Compose`) with run/bind.
* `Free.Agent.Pipeline` — pure stages; route helpers; `forName`/`fromName`.
* `Free.Agent.Host` — oneshot host + `processHost` sketch (no hermes dep).
* `Free.Agent.Seat` — free Pipeline+Host seats; `interpretSeat` via `composeEnds`.
* `free-agent-axioma` — layer/pipeline/seat/route/process oracles.
