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

```bash
cabal build free-agent free-agent-axioma
cabal run free-agent-axioma
cabal haddock
```
