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
cabal run free-agent -- bus say -r /tmp/free-agent-demo --from alice --to demo "hello"
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
cabal run free-agent -- bus say -r /tmp/free-agent-demo --from alice --to demo "ping"
```

For serialized stamping, run the daemon in one terminal:

```bash
cabal run free-agent -- bus daemon -r /tmp/free-agent-demo
```

Post through its FIFO from another terminal:

```bash
cabal run free-agent -- bus post -r /tmp/free-agent-demo --via-daemon --from alice --to demo "stamped by the daemon"
```

Without the daemon, `bus say` and `bus post` self-stamp under the log lock. With the daemon, writers submit bare post lines through `bus.fifo` and the daemon assigns ids in arrival order.

## Bus commands

```text
free-agent bus say [-r ROOT] BODY... [--from NAME] [--to NAME]... [--thread ID]
free-agent bus post [-r ROOT] [--from NAME] [--to NAME]... [--body TEXT] [--via-daemon]
free-agent bus read [-r ROOT] [NAME...] [--since ID | --cursor NAME | --thread ID] [--depth 0|1|2]
free-agent bus watch [-r ROOT] NAME...
free-agent bus cursor [-r ROOT] get NAME
free-agent bus cursor [-r ROOT] set NAME ID
free-agent bus ping-watch [-r ROOT] NAME
free-agent bus status [-r ROOT] [--threshold SECS] [--live] [--depth 0|1|2]
free-agent bus daemon [-r ROOT]
```

Useful entry points:

- `say` is the short act path for humans; body words can also be piped on stdin.
- `read` prints posts addressed to the named seats, a thread, or everything at and after a cursor.
- `watch` follows addressed posts as they arrive.
- `cursor get` and `cursor set` inspect or repair a seat's resume position.
- `ping-watch` waits for the next change to a seat's ping latch.
- `status` reports post count, last post, cursor state, and optionally live seat heartbeats.
- `daemon` runs the single-writer stamping office for the bus root.

## Delivery and publication

A delivered prompt produces a local response, not an automatic bus reply. `cliDeliverTo` runs one interruptible CLI turn and records its response and causing post id in the existing transcript. Plain response text stays local. Hermes and DSH adapters publish only an explicit `@name: response` reply act, which carries the causing post's channel and ancestry.

Each executable seat has a finite `--delivery-budget N` (default 1000000). Exhaustion stands the loop down without invoking the backend or advancing the causing cursor.

```text
free-agent -r ROOT --name NAME --prompt PROMPT.md [--backend hermes|kimi|gateway] [--delivery-budget N]
free-agent dsh -r ROOT --name NAME --prompt PROMPT.md --dsh-patch PATCH.yml [--dsh-model-patch MODEL.yml] [--delivery-budget N]
free-agent llm -r ROOT --name NAME --prompt PROMPT.md [--base-url URL] [--key-env NAME] [--delivery-budget N]
free-agent acp -r ROOT --name NAME --prompt PROMPT.md [--acp-cmd CMD] [--delivery-budget N]
free-agent cmd -r ROOT --name NAME --cmd CMD [ARG...] [--delivery-budget N]
```

`llm` uses `bareHost`, `gateway` targets a Hermes api_server, `acp` speaks JSON-RPC to a persistent child such as `kimi acp`, and `cmd` treats an external command as the seat.

## Library

The package sits above circuits-agent and owns the bus file format, delivery and response settlement, cursors, ping latches, heartbeats, host constructors, and the common seat loop. It is coordination plumbing, not a model-quality layer: memory, tools, and prompt context belong to the host harness.

Main modules:

- `Free.Agent.Bus` — append-only JSONL bus, file locking, pings, cursors, and the in-process bus image.
- `Free.Agent.Bus.Cli` — the `free-agent bus` command surface.
- `Free.Agent.Bus.Daemon` — the optional single-writer stamping daemon.
- `Free.Agent.Bus.File` — file cursors and the fsnotify tail loop.
- `Free.Agent.Bus.Heartbeat` — seat liveness files used by `status --live`.
- `Free.Agent.Delivery` — open-cause turns, cancellation, bounded grants, and explicit reply acts.
- `Free.Agent.Cli` — interruptible CLI delivery with per-delivery transcript causation.
- `Free.Agent.Host` — generic host execution and response mapping.
- `Free.Agent.Agent.Runner` — the bounded long-running seat loop shared by the executable backends.

## Development

```bash
cabal build free-agent free-agent-axioma
cabal run free-agent-axioma
```

`free-agent-axioma` pins the bus and seat behavior with executable witnesses. Experiment probes and measurement harnesses live outside this package in the surrounding workspace.
