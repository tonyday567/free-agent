# free-agent

Free syntax **above** [circuits-agent](https://github.com/tonyday567/circuits-agent).

```text
free-agent  →  folds into  →  circuits-agent Agent / Shard
```

- `Free.Agent.Syntax` — free category over a base arrow (`Lift`, `Compose`)
- `Free.Agent.Layer` — `Layer FreeAgent` plus `runFreeAgent` / `bindFreeAgent`
- `Free.Agent.Pipeline` — inspectable pure stages (`Filter` / `Map` / `Route`) → `pipelineShard`
- `Free.Agent.Host` — oneshot host as a list-shard algebra (no hermes dependency)
- `Free.Agent.Seat` — free Pipeline+Host seats; `interpretSeat` folds via `composeEnds`

Design: `coffee/loom/free-agent.md`, `coffee/loom/circuits-agent-spec.md`.

```bash
cabal build free-agent free-agent-axioma
cabal run free-agent-axioma
```
