# free-agent

free-agent is a small Haskell runtime for named agents that coordinate through an append-only bus.

A bus is a directory with a `log.jsonl` file. Every line is a stamped JSON post with `from`, `to`, `thread`, and `body` fields. A participant, called a seat, subscribes to one or more names and keeps a cursor file `.cursor-NAME` holding the next post id to process. Posts addressed to a seat also update its ping latch `.ping-NAME` with the latest post id, so a waiter can block on one small file instead of reparsing the log. Replies are appended as new posts; the log is the shared state.

Status: active development. The package version is 0.1.0.0 with unreleased post-0.1.0.0 changes; GHC 9.10.1, 9.12.2, and 9.14.1 are in the tested range. The bus CLI and hosted seats are in live use, but command shapes and library APIs may still move.

## Quick start

Build the CLI and the oracle executable:

```bash
cabal build free-agent free-agent-axioma
```

Create a bus root:

```bash
mkdir -p /tmp/free-agent-demo
: > /tmp/free-agent-demo/log.jsonl
```

Post to it and read the log:

```bash
cabal run free-agent -- bus post -r /tmp/free-agent-demo --from alice --to demo --body "hello"
cabal run free-agent -- bus read -r /tmp/free-agent-demo demo --depth 1
cabal run free-agent -- bus status -r /tmp/free-agent-demo
```

Run a Hermes-backed seat:

```bash
printf '# demo agent\nAnswer briefly.\n' > /tmp/free-agent-demo/demo.md
cabal run free-agent -- -r /tmp/free-agent-demo -n demo -p /tmp/free-agent-demo/demo.md --backend hermes
```

From another terminal, wake it:

```bash
cabal run free-agent -- bus post -r /tmp/free-agent-demo --from alice --to demo --body "ping"
```

For serialized stamping, run the daemon in one terminal:

```bash
cabal run free-agent -- bus daemon -r /tmp/free-agent-demo
```

Post through its FIFO from another terminal:

```bash
cabal run free-agent -- bus post -r /tmp/free-agent-demo --via-daemon --from alice --to demo --body "stamped by the daemon"
```

Without the daemon, `bus post` self-stamps under the log lock. With the daemon, writers submit bare post lines through `bus.fifo` and the daemon assigns ids in arrival order.

`bus say` was removed on 6 Nov 2026: it was a second way to write the same post. The surviving form carries everything it did, plus a channel:

```bash
# body from the flag, ancestry and channel named
free-agent bus post -r BUS --from NAME --to NAME --body TEXT --thread ID --channel NAME
# body piped (the form `bus say` used to take)
printf '%s' "$TEXT" | free-agent bus post -r BUS --from NAME --to NAME --thread ID
# a whole post line on stdin, ancestry and channel as the caller wrote them
printf '%s' "$JSON" | free-agent bus post -r BUS
```

## Bus commands

```text
free-agent bus post [-r|--root ROOT] [--from NAME] [--to NAME] [--body TEXT] [--thread ID] [--channel NAME] [--via-daemon]
free-agent bus watch [-r|--root ROOT] NAME...
free-agent bus read [-r|--root ROOT] [NAME...] [--since ID | --cursor NAME] [--thread ID] [--depth 0|1|2]
free-agent bus inbox [-r|--root ROOT] NAME [--mark-read] [--depth 0|1|2]
free-agent bus attach [-r|--root ROOT] NAME
free-agent bus detach [-r|--root ROOT] NAME
free-agent bus retire [-r|--root ROOT] NAME
free-agent bus ready [-r|--root ROOT] [--depth 0|1|2]
free-agent bus cursor get [-r|--root ROOT] NAME
free-agent bus cursor set [-r|--root ROOT] NAME ID
free-agent bus ping-watch [-r|--root ROOT] NAME
free-agent bus status [-r|--root ROOT] [-t|--threshold SECS] [--depth 0|1|2] [--org]
free-agent bus daemon [-r|--root ROOT]
```

Regenerated from `free-agent bus --help`; the help text is the source of truth.

Useful entry points:

- `post` submits flag posts as roots; JSON posts may carry thread ancestry; its body comes from `--body` or is piped on stdin.
- `read` prints posts addressed to the named seats, a thread, or everything at and after a cursor.
- `inbox` prints unread posts addressed to NAME; `--mark-read` advances `.cursor-NAME` to the last printed id.
- `ready` lists unclaimed open TASK offers with no unresolved blocker. Directed TASKs are assigned at creation; first CLAIM owns an open offer, and only the owner's DONE completes it.
- `watch` follows addressed posts as they arrive.
- `attach` / `detach` record or remove NAME's attached-human marker — evidence only; no delivery or READY path changes.
- `retire` writes the dead marker to NAME's heartbeat: the seat joins the black retired circle, its cursor, log and transcripts stay as evidence; no process is signalled.
- `cursor get` and `cursor set` inspect or repair a seat's resume position.
- `ping-watch` waits for the next change to a seat's ping latch.
- `status` reads one snapshot of circle membership, directed unread mail, and last posts. The circles are 🟣 working, 🟢 ready, 🟡 parked, 🔴 crashed, ⚫ retired, ⚪ non-fab. Depth 0 prints the census, depth 1 adds rows, depth 2 adds raw evidence; `--org` renders the same snapshot for fab.
- `daemon` runs the single-writer stamping office for the bus root.

## Delivery and publication

A delivered prompt produces a local response before publication. `cliDeliverTo` runs one interruptible CLI turn and records its response and causing post id in the existing transcript. Publication is explicit, never inferred: a response publishes only when it opens with an `@name:` address, and it goes to that name. Unaddressed prose stays local, and a first line of `LOCAL` suppresses publication. A published reply carries no inferred cause, channel or ancestry (`replyAct`, `src/Free/Agent/Delivery.hs`); a turn that wants a thread cites a visible post id through an explicit bus act, `free-agent bus post --thread`. An address on a later line publishes only its addressed suffix, and warns with the address's character offset.

Each executable seat has a finite `--delivery-budget N` (default 1000000). Exhaustion stands the loop down without invoking the backend or advancing the causing cursor.

```text
free-agent -r ROOT --name NAME --prompt PROMPT.md [--backend hermes|kimi|gateway] [--delivery-budget N]
free-agent dsh -r ROOT --name NAME --prompt PROMPT.md --dsh-patch PATCH.yml [--dsh-model-patch MODEL.yml] [--delivery-budget N]
free-agent llm -r ROOT --name NAME --prompt PROMPT.md [--base-url URL] [--key-env NAME] [--delivery-budget N]
free-agent acp -r ROOT --name NAME --prompt PROMPT.md [--acp-cmd CMD] [--delivery-budget N]
free-agent cmd -r ROOT --name NAME --cmd CMD [ARG...] [--delivery-budget N]
```

`llm` uses `bareHost`, `gateway` targets a Hermes api_server, `acp` speaks JSON-RPC to a persistent child such as `kimi acp`, and `cmd` treats an external command as the seat.

Seat operations — cold start, reboot recipes, liveness readings, failure repair — live in [seats.md](seats.md).

## Library

The package sits above circuits-agent and owns the bus file format, delivery and response settlement, cursors, ping latches, heartbeats, host constructors, and the common seat loop. It is coordination plumbing, not a model-quality layer: memory, tools, and prompt context belong to the host harness.

Main modules:

- `Free.Agent.Bus` — append-only JSONL bus, file locking, pings, cursors, and the in-process bus image.
- `Free.Agent.Bus.Cli` — the `free-agent bus` command surface.
- `Free.Agent.Bus.Daemon` — the optional single-writer stamping daemon.
- `Free.Agent.Bus.File` — file cursors and the fsnotify tail loop.
- `Free.Agent.Bus.Heartbeat` — seat liveness files used by `bus status`.
- `Free.Agent.Delivery` — open-cause turns, cancellation, bounded grants, and explicit reply acts.
- `Free.Agent.Cli` — interruptible CLI delivery with per-delivery transcript causation.
- `Free.Agent.Host` — generic host execution and response mapping.
- `Free.Agent.Agent.Runner` — the bounded long-running seat loop shared by the executable backends.

## Development

`fare gate` reads the repository's optional `.fare` file for its default
docspec lane: `gate.docspec = none`, `repository`, or `full`. This checkout
selects `repository`, so bare `fare gate` runs build, ormolu, hlint, and
doctests for `free-agent`; linked local projects remain excluded and the
verdict says so. `--docspec`, `--docspec-full`, and `--no-docspec` override
the configured default for one invocation.

```bash
cabal build free-agent free-agent-axioma
cabal run free-agent-axioma
```

`free-agent-axioma` pins the bus and seat behavior with executable witnesses. Experiment probes and measurement harnesses live outside this package in the surrounding workspace.

The restricted tool menu for agent seats — `fare` — is documented in [fare.md](fare.md).
