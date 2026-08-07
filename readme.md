# free-agent

Free syntax **above** [circuits-agent](https://github.com/tonyday567/circuits-agent).

```text
free-agent  →  folds into  →  circuits-agent Agent / Shard
```

Package role **ABOVE**: depends on circuits-agent; does not replace `Post` /
`Agent` / `Shard`.

| Module | Role |
|--------|------|
| `Free.Agent.Syntax` | Free category over a base arrow (`Lift`, `Compose`) |
| `Free.Agent.Layer` | `Layer FreeAgent` + `runFreeAgent` / `bindFreeAgent` |
| `Free.Agent.Pipeline` | Pure stages + `Category`; route / `forName` / `fromName` → `pipelineShard` |
| `Free.Agent.Host` | Oneshot host + `processHost`; `hostShard` emits per committed post |
| `Free.Agent.Seat` | Free Pipeline+Host seats; `interpretSeat` → `Shard (StateT [Post] IO)` via `composeEnds` |

Design notes: coffee loom `free-agent.md`, `circuits-agent-spec.md` §8b.

## executables

The library ships one operational CLI:

- `free-agent` — bus tools, LLM seats, and log status
  - `free-agent` — Hermes-backed seat (default)
  - `free-agent --backend kimi` — Kimi-backed seat
  - `free-agent llm` — direct API seat
  - `free-agent cmd` — external-command seat
  - `free-agent bus [SUBCOMMAND]` — JSONL scribe/CLI
  - `free-agent status LOG.jsonl` — log metrics

The axioma/oracle executable is `free-agent-axioma`.

Experiment harnesses and backend-specific probes live separately under
`~/machina/free-agent-machina/` so measurement tooling does not burden the
published library. That package includes `process-pair`, an Abbott & Costello
dialogue backed by simple Perl scripts — a minimal example and oracle that the
`processHost` / seat wiring works end-to-end with an external command.

```bash
cabal build free-agent free-agent-axioma
cabal run free-agent-axioma
cabal haddock
```
