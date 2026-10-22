# Revision history for free-agent

## unreleased

- **Wave X: the seat term language is gone.** `Free.Agent.Seat` (`FreeSeat`,
  `interpretSeat`, `interpretSeatA`, `interpretSeatS`, the combinators and
  the STM rung) and `Free.Agent.Pipeline` pinned an inspectable term
  algebra that no production path used: every live seat was a bare
  `SeatHost` erased by `interpretSeat` immediately at the call site, and
  `runSeatBus` was the only consumer of the seat fold. Both modules are
  deleted; `runSeatBus` now takes the `AgentShard` from circuits-agent
  directly, the seven backend handlers in `free-agent.hs` call
  `hostShard` directly, and the oracles that pinned surviving behavior
  (host shard round-trips, the CLI/process host sketches, runSeatBus
  supervision/self-halt/resume/empty-reply semantics, L5 persister death)
  are re-pointed at `hostShard` / a silent `ioShard`. The `bundle` and
  `stm` axioma topics die with the term language, as do the seat-fold
  sections of the `seat` topic (pipeline laws, FreeSeat close/assoc,
  fork-is-id, route-by-name). Suite: 266 → 221 PASS (45 dead-term oracles;
  zero surviving assertions lost).

- **Lifecycle hygiene (wave H).** The B3 watermark hang and the daemon
  receipt crash had three distinct mechanisms, all fixed at the root.
  (1) The log lock no longer wedges the runtime: `withLogLock` replaces
  the blocking `flock` with a `withTryFileLock` spin (bounded, loud at
  ~10s) at all four lock sites — a same-process waiter can no longer
  sleep an OS thread in the C call while the holder waits to be
  rescheduled. (2) GHC's RTS locks files in-process per (dev,ino) and
  the modes conflict fatally (Append-vs-Append and Read-vs-Append both
  throw "resource busy"): a process-wide mutex (`withBusFiles`) now
  serializes every bus-path open/act/close — appends and touches under
  `withLogLock`, drains via `withLogRead`, strict reads via
  `readFileStrict` (all lazy `TIO.readFile` spellings of bus paths are
  gone; `Framing.readLogFile` remains for other packages). (3) The
  stamping daemon cannot die on a receipt write: bounded retries then a
  stderr report — the receipt is a courtesy, the log is truth — and
  `postViaDaemon` fails fast naming the daemon and the root when no
  live daemon reads the fifo (ENXIO is the expected tell), with the
  receipt timeout as mid-flight backstop. `ensureFifo` makes fifo
  creation idempotent: already-exists passes only for a real named
  pipe. `awaitSince` observes a `Failed` persister (`Left`, mirroring
  `awaitDurable`) and `runSeatBus` halts with the persister's exit
  reason instead of retrying forever. The persister reconciles external
  growth on a one-second tick — a quiet bus's image no longer lags the
  file until the next local scribe. `free-agent-axioma` builds
  `-threaded` (defense in depth; the lock fixes are what make it safe);
  the fork-first seat oracles (M1a, Q7) and B3's reconcile check are
  deterministic under true parallelism — cursor pins and a batch plant
  replace the scheduling accidents they encoded. Every axioma temp root
  is per-run unique (`Axioma.Common.tempRoot`), killing the
  cross-instance class: orphaned fifos, truncated logs, false-matched
  receipts. New `lifecycle` topic: L1 lock stress, L2 receipt stress,
  L3 dead-daemon honesty, L4 fifo double-create, L5 persister death
  ends seats.
- **Redundancy sweep (R2b).** `Free.Agent.Derivation` renamed to
  `Free.Agent.Lineage` (the dPost/dParents square; `Circuit.Agent` keeps
  `Derivation`). Dedupes: `writePings` one spelling in `Bus`, the dead
  3-arg `hermesCli`, `jObjLookup` onto `Json.objLookup`, `scrubReply`
  folded into `cleanCliOut`, `rootOpt` one spelling. Binned: `runConnector`
  + `TurnResult` (`runTurn` stays, oracled), `cliQueryBS`, `cliShard`, the
  non-batch `hermesHost`, the fare usage/pc/bash stubs, and
  `Free.Agent.Syntax`/`Layer`/`Hyper` with their oracles (runner verdict).
  Cabal: `-Wno-unused-imports` and the vestigial `mtl` dep dropped, the
  exposed unused imports cleaned. The audience default is pinned: a seat's
  reply to an `all`-post addresses the sender only.
- **ACP seat wired (R2a).** `free-agent acp` runs a persistent ACP child
  (`kimi acp` by default, `--acp-cmd` to override) as a bus seat: one
  `session/new` at startup, one `session/prompt` per ask — the multi-turn
  repl the one-shot CLI seats cannot be.  `Free.Agent.Acp` gains the
  `acpHost` seat constructor; the client lifecycle (open/initialize/new/
  close) rides the runner like the gateway seat.  Pinned by two oracles: a
  scripted NDJSON peer on the S-a double (protocol precision) and the wired
  binary against a fake peer on real pipes (integration).  A live
  `kimi acp` probe remains the runner's seat.
- **The seam oracle (Q7).** One deterministic index-citing seat runs
  through the pure calculus `loop` (circuits-agent) and the operational
  `runAgentLoop`; the final logs are the same `[Post Text]` once stamps
  are stripped — ancestry edges, mark control, and log order all agree.
  The pure `Log` is newest-first (seed it newest-first; `watch` is the
  oldest-first reading), and the cursor is pinned to 0 up front — the
  cold-boot default would skip the script. Pinned by the Q7 oracles in
  `free-agent-axioma` (mutation-verified against the pre-M1 no-ancestry
  runner).
- **Downstream of death honesty.** The Connector and ACP seats read the
  new terminal `Frame`s from the process ports: a dead repl is a
  `<exited N>` diagnostic turn (content-decided halt) instead of a silent
  hang, startup death stops the connector before it tails, and stderr
  drains end at the terminal frame. The axioma's mock proc-ends deliver
  `Payload` frames. Adapted to circuits-agent's S-a commit.
- **Reply ancestry (M1a).** `runAgentLoop` now cites the delivered post's
  stamped id in every post it produces: handler replies prepend the parent
  id to their thread (`runSeatBus` parity), and both 🔴 relays — the
  handler-failure escalation and the incoming-escalation relay to the
  pitboss — carry the edge to the ask they answer. Pinned by the M1a
  oracles in `free-agent-axioma`.
- **Watermark persister.** The scribe ack (a timeout-less `takeTMVar`
  handoff) is off the agent path: the persister is a seat over a
  `busDurable` watermark, `awaitDurable`/`awaitQuiescent` end by content
  (durable or `Failed`), and any persister exit writes `Failed` — death is
  content. `readSince`/`awaitSince` see only the persisted prefix, so a
  seat can never react to a post that isn't on disk; external growth on a
  shared root is reconciled (pulled into the image, invisible tail
  renumbered) so ids stay file-positional. Pinned by the B1-B3 oracles in
  `free-agent-axioma`.
- **Quiescence rewrite.** `tailLog`'s quiesce path is now a pure Moore
  machine (`quiesceAgent` over `QEvent`) with an IO pump: a silent bus
  stands down (the `busy` gate is gone), chatter no longer starves the
  counter, every wake re-drains so a missed FSEvent cannot strand an
  addressed post, a post-registration drain closes the startup gap, and
  the signal starts empty. Timeout stand-downs post ⌛ (`Timeout`), not 🔵 —
  an inference from wall-clock silence, not a decided quiet. Pinned by the
  Q1-Q5 oracles in `free-agent-axioma`.
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
