# Revision history for free-agent

## 0.1.0.0 -- 2026-08-01

* Package role **ABOVE** circuits-agent: free syntax that folds into Agent/Shard.
* `Free.Agent.Syntax` / `Layer` — free category (`Lift`/`Compose`) with run/bind.
* `Free.Agent.Pipeline` — pure stages; route helpers; `forName`/`fromName`.
* `Free.Agent.Host` — oneshot host + `processHost` sketch (no hermes dep).
* `Free.Agent.Seat` — free Pipeline+Host seats; `interpretSeat` via `composeEnds`.
* `free-agent-axioma` — layer/pipeline/seat/route/process oracles.
