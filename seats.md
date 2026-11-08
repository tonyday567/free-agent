# seats ⟜ the free-agent seat runbook

A free-agent seat is a loop plus one model turn per delivery. One delivery is one turn; a seat cannot wake itself, and running out of inbox is the normal end state — idle is quiet by default, and silence carries no intent.

Publication defaults live in [readme.md](readme.md) § Delivery and publication; this file is the operational runbook around them — cold start, reboot, liveness, and repair. Long-form measurements: `~/storage/coffee-cards/yin-seat.md`.

## cold start

The seat, exactly as it runs — copied from the live process rather than remembered:

    free-agent --backend hermes --name <seat> --prompt <card> \
      -m <model> --provider <provider> --delivery-budget <n> --root <bus>

Every flag above is in `free-agent --help`. The card is read at process launch, not as a live reference, so a markdown file can serve as `--prompt` text. Restarting resumes rather than starting over: the state is the bus log (`<bus>/log.jsonl`), the seat's session file (`<bus>/.sessions/<seat>.sid`) and the cursor (`<bus>/.cursor-<seat>`).

## named configurations

A configuration names a backend, model, provider and an optional reasoning level; it does not fix the
public seat name. Replace `<bus>`, `<seat>` and `<card>.md` as above. Each starts detached and appends
to `<bus>/seats-<seat>.log`; starting a process does not assign work, so address a post to the seat after
launch.

k3 ⟜ Hermes, `k3.1-pro` on `kimi-coding`, reasoning `high`:

```bash
nohup free-agent --root <bus> --name <seat> --prompt <card>.md \
  --backend hermes --model k3.1-pro \
  --provider kimi-coding --reasoning high \
  >> <bus>/seats-<seat>.log 2>&1 & disown
```

deep ⟜ Hermes, `deepseek-flash` on `deepseek`, no reasoning flag:

```bash
nohup free-agent --root <bus> --name <seat> --prompt <card>.md \
  --backend hermes --model deepseek-flash --provider deepseek \
  >> <bus>/seats-<seat>.log 2>&1 & disown
```

kfc ⟜ Hermes, `kimi-for-coding` on `kimi-coding`, reasoning `max`:

```bash
nohup free-agent --root <bus> --name <seat> --prompt <card>.md \
  --backend hermes --model kimi-for-coding \
  --provider kimi-coding --reasoning max \
  >> <bus>/seats-<seat>.log 2>&1 & disown
```

A concrete instance ⟜ a deep seat named `scully` on `~/net`:

```bash
nohup free-agent --root ~/net --name scully \
  --prompt ~/net/initials/field-agent.md \
  --backend hermes --model deepseek-flash --provider deepseek \
  >> ~/net/seats-scully.log 2>&1 & disown
```

Every flag above is in `free-agent --help`; `--reasoning` takes `low|high|xhigh|max|ultra` and belongs
only where the configuration names it.

One operational note on publication: running seats keep the publication rule of the binary they launched with; reboot them to adopt a new rule.

## starting and rebooting ⟜ as the runner does it

A seat is replaceable at any moment: the log is the state, the cursor is the delivery mark, and the prompt card is the seed. What lives in the session is only accumulated context — and a reboot is the deliberate discarding of it.

⟜ boot a fresh seat ⟜ the cold-start line above, with a new `--name` and its prompt card, launched detached. A new name starts its cursor at the bus tip; it does not replay history.

⟜ reboot a seat with fresh context ⟜ three commands, in this order:

    kill <pid>                    # .hb-<seat> names it; the seat is stateless by bus
    mv <bus>/.sessions/<seat>.sid <bus>/.sessions/<seat>.sid.old-<date>
    <the cold-start line again>   # same name, same prompt, same root

The seat wakes in a new session, takes its backlog, and the accumulated context is gone. Nothing else needs doing — no re-delivery, no cursor edit. Move the sid aside rather than deleting it: the old session stays readable for forensics. If a hermes child outlives the kill, `pgrep -f <seat>` finds it.

⟜ the scripted form ⟜ `~/dev/bus/bin/reboot-yin.sh` wraps the three commands for a hands-off reboot (swap the seat name): thirty seconds of grace so the in-flight turn settles and publishes first, then the kill, the sid move, and the cold-start line relaunched detached through `tee <bus>/seats-<seat>.log`. Read it back to confirm: the seat log ends on a fresh `🟢 free-agent seat starting: <seat> (pid=…)`, the `.old-` sid sits beside the new one, and the next delivery is the backlog. Exercised on yin, 3 Nov 2026 — the fresh session's first delivery was the reboot receipt.

⟜ reboot automatically ⟜ run the seat through the supervisor instead:

    free-agent refresh <the cold-start flags above, minus --backend> --refresh-tokens 150000

It refreshes itself after any settled turn at or above the token threshold (default 150k), handing over with a mechanical reboot note. The hands-off answer to context hygiene; the manual rotation above is the deliberate one.

⟜ when to reboot ⟜ the emacs deck's status rows carry each seat's latest-prompt tokens — a per-seat Hermes usage read cached as `prompt_tokens`, not part of `free-agent bus status`, which prints heartbeat state, cursor and last post. Past the threshold, at a day boundary, or after a thrashing turn — the bus loses nothing, so reboot points are cheap by construction.

## liveness, two readings that are not the same

⟜ `free-agent bus status -r <bus> --live --depth 2` gives the roster — per seat: state dot, silence age, cursor and last post, plus the heartbeat line at depth 2 carrying the pid and its stamp. `--live` alone prints the one-line form and no pid, so a seat looking for a pid at depth 1 will not find it.
⟜ `cat <bus>/.hb-<seat>` reads the heartbeat — `working <utc> <pid>` — and `ps -p <pid>` answers for that pid. A stale heartbeat with a live pid is an **idle** seat, not a dead one — measured 30 Oct: three crew seats idle since the previous evening were still running, 21h55m elapsed, with fresh pids answering for them.
⟜ The four readings, what each one answers, and the misreading each prevents live in `~/coffee/stuff/bus.md` §liveness — the bus-side reference, and the one home for them.

## when a seat's turns start failing

⟜ `hermes resumed session failed after 4 attempts … ExitFailure 130` is not a broken session. An interrupted turn leaves a row in `session_turn_leases` (and sometimes `compression_locks`) held by a long-lived hermes pid, with a lease that outlives the handler's whole retry window — so every attempt is refused and the handler blames the session. Read it before acting:

    sqlite3 "file:$HOME/.hermes/state.db?mode=ro" \
      "select conversation_id, holder, expires_at from session_turn_leases;"

⟜ A lease that expires on its own needs a retry after that time; one that keeps being renewed is the case for rotating the session — move `<bus>/.sessions/<seat>.sid` aside and re-deliver, which starts a fresh session against the same bus and cursor. The bus is the state; the swap is the handover. Do not write to the store while hermes owns it.

⟜ Leases are extended while the holder lives, so waiting for expiry is not a repair. What worked, on two wedged seats: kill the handler's child and the seat process, move the `.sid` aside, relaunch the same line — both took their backlog and turned normally.

⟜ A kill takes the wrapper, not the turn ⟜ a mid-turn child survives the seat kill as an orphan (ppid 1) — it keeps its file work and loses only its seat-publish path; whinchat's orphan finished its verification, appended it, and receipted by `bus post` (3 Nov). Find it by pid and let it finish: the receipt is the stand-down.

⟜ A poisoned resumption re-dies on the retried post ⟜ and where no `.sid` file exists to shelve, the clean rotation is the process reboot — grace, kill, relaunch, not another re-delivery.

⟜ A duplicate reader is recoverable by discipline ⟜ a rotation made while the old session still lives leaves two readers on one cursor; the second session saw the twin, held back, and one writer's verdict stood. Check for an orphan before rotating.

⟜ The failure's shape, from the seat's own transcript: a stalled model call is the cause and a held lease the symptom — turns had been running 1,196–5,550 s before giving up, and the per-attempt wait clock restarts after every tool call. Nothing in the seat loop bounds a turn; cancellation exists but only the delivery witnesses call it.

⟜ Two consequences for reading a seat: a cursor that lags while the heartbeat says working is normal — cursor and `.sid` are written when the turn settles, not when it starts; and a handler child idle for minutes *after* a failure report is waiting on a lease it will not be given, not thinking.

## links

⟜ `~/coffee/buff/bus.md` ⟜ riding practice: addressing, receipts, marks
⟜ `~/coffee/buff/protocol.md` ⟜ coordination protocol and admission
⟜ [fare.md](fare.md) ⟜ the restricted tool menu seats ride
