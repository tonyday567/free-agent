# walks

Behaviour walks for the bus CLI. The runner had been the bus's only test suite
and the wounds found the gaps; these are for development to run instead.

    walks/run.sh              the committed suite
    walks/run.sh --all        everything, dirty files included
    walks/run.sh 'cli-*'      one family
    bash walks/cli-inbox-flow.sh

`walks/run.sh` runs the **committed** suite by default: a walk whose file, or
`lib.sh` itself, differs from `HEAD` is listed and skipped, because a red tally should
mean the handed-in state failed rather than that an author is mid-edit. `--all` runs the
dirty files too, which is what an author wants while writing one.

Each walk sets up its own scratch root under `~/.hermes/cache/scratch/deck-walks/`,
seeds it, invokes the CLI, and prints expected versus actual per step. PASS/FAIL,
non-zero exit on failure.

Scratch only: never the runner's live root or live daemon. CLI walks need no
daemon at all.

## a walk that hangs

Each walk gets 120 seconds of wall clock (`WALK_TIMEOUT` seconds to change it) and a kill
is counted as a failure, with its own line: `FAIL the walk hung and was killed after Ns`.
A walk that never returns would otherwise take the whole tally with it — measured 31 Oct
when a ping-watch walk died mid-run and the run printed no summary at all.

## exit codes

⟜ `0` ⟜ every committed walk passed.
⟜ `1` ⟜ a walk failed.
⟜ `2` ⟜ nothing ran, because every walk was dirty or new. A wrapper must not read that as
  success, which is why it is not 0.

Skipped walks are listed with their reason: `work in progress` for a modified file, `new,
not committed` for an untracked one. If `lib.sh` itself is uncommitted, every walk is
skipped with that reason, since they would all be running against uncommitted helpers.

## installing, and what the tally is a fact about

Walks run against whatever `FA` points at, defaulting to the `free-agent` on `PATH` - the
installed binary. So the shared tally is a fact about the installed binary, and installing
mid-repair makes it a fact about half-finished work instead. The convention that follows:
`cabal build`, probe against the binary under `dist-newstyle` by path, and install only
when the suite is green. A walk run can exercise the dist build directly the same way -
`FA=<dist path> bash walks/run.sh` - so a seat can verify against work that is built but
not installed, which is where yang landed on this after a WIP install reddened the shared
number twice.

## waiting on a watcher

A walk that waits for a watcher must converge on it, not sleep at it. FSEvents
registration is not observable from outside — a write inside the registration gap is
missed entirely and never replayed, and a write that lands before registration can
arrive historically. So a sleep is a race the bound will eventually catch under load,
and the honest patterns are: poll for the child's exit, arm a second write if the first
did not take, and trap EXIT so no watcher outlives a failed run. `cli-ping-watch.sh`
carries the long form in its header, after a flake that cost a morning.

## the two assertions

⟜ `assert_eq` ⟜ the behaviour we want. A failure is a bug to fix.
⟜ `assert_known` ⟜ behaviour that is wrong and currently true, written down with its
  reason. It passes while the behaviour holds and fails the moment it changes, so
  fixing a defect turns the walk red and the assertion gets promoted to `assert_eq`.

## the walks

⟜ `cli-inbox-flow.sh` ⟜ unread is id >= cursor, `--mark-read` sets the cursor to the
highest printed id plus one, a caught-up name prints nothing and exits 0.
⟜ `cli-inbox-unset-cursor.sh` ⟜ the wound behind the runner's inbox reading zero: a
name with mail and no cursor file receives nothing, silently, exit 0. Repaired the same
day: the walk now pins the repaired behaviour end to end.
⟜ `cli-say-addressing.sh` ⟜ who a post is for: `--to NAME` delivers; an omitted `--to`
posts publicly rather than being refused; `--to ""` is refused by name since `fcce8b0`;
ancestry is carried verbatim, including dangling ids, and the read of a dangling id is
where it fails loudly.
⟜ `cli-retire.sh` ⟜ a retired seat leaves the live roster and keeps its cursor, mail
and log; no process is signalled.
⟜ `cli-watch.sh` ⟜ a watcher skips pre-watch history, prints only new addressed posts
as raw JSON frames, creates no cursor, keeps waiting after delivery, and leaves no
process when killed. `watch` has no `--depth` option.

## a run that measured nothing is not a green run

The same rule as nothing-ran-is-2, one level down, found on a live seat harness rather than
a walk: a fresh seat with no `.cursor-NAME` starts at the log tip, so every seeded post is
already behind it, nothing is delivered, the seat stands down after two empty cycles and the
harness exits **0**. The run measured nothing and called it success. A live harness has to
write the cursor before it launches, and that write is part of the harness rather than a
detail — shen hit this on the first attempt at the strand-bug acceptance and recorded it
with its receipt (`73f5c96`).

Reproduced here independently against the installed binary, which is the shape worth
copying: a scratch root, one seeded post `alice → bob` at id 0, `.cursor-bob` written to 0,
and a four-line `kimi` script on `PATH` standing in for the model — `kimiCli` invokes
`kimi -p PROMPT`, so the shim is the whole model. The shim printed a bar-prefixed diff, an
added line, then `@alice: …`; the seat published only the addressed suffix, kept ancestry,
and warned once on stderr (`begins at character offset 33, not 0; publishing addressed
suffix`), exactly as the oracle pins it. A second harness trap, and this one is shared
rather than either seat's: the observation is mine, a scratch seat that had to be killed at
45 seconds and exited 124, and the cause is shen's, sharper than my first reading of it -
with no `--quiesce N` there is no quiet-exit bridge at all -
`quiesceOpt` is `Maybe Int` and the quiesce action errors without config - so the loop runs
until the delivery budget, whose default is a million deliveries. A scratch seat launched
without `--quiesce` is never going to stand down, and the harness killing it is the harness
doing its job rather than a difference in behaviour. What that run does not reach, as the
receipt says: the model is a script, so no real tool call is made by a harness, and the diff
is only what the adapter sees on the pipe.

## the knowns, as of 31 Oct

Wrong behaviour that the walks assert, so a fix turns them red instead of passing
quietly. One survives, measured by the walk that keeps it.

⟜ **an omitted `--to` posts to nobody** ⟜ it writes `to: []`, and `deliversTo` answers
False for it: `read NAME` and `inbox NAME` show only genuinely addressed posts. The walk
asserts this as a known rather than an expectation, because the log view is what calls
such a post public while delivery reaches no seat. The empty-name half is a different
story and is refused (`1e68fa3`).

## the repairs, on the same day

Six behaviours these walks caught, fixed and flipped from known to assertion in one day.
Listed here rather than deleted, because the shape of the day is the argument for keeping
the walks.

⟜ `--to ""` wrote a recipient named the empty string, a third state nothing delivered and
nothing warned about — refused by name, `1e68fa3`; a mixed `--to bob --to ""` is refused
whole rather than landing half a post.
⟜ `bus read` on a root with no `log.jsonl` exited 0 in silence while every other surface
refused — `requireBus`, `ced433d`.
⟜ `status` refused a non-bus root without the repair hint the other four surfaces carry —
one message for all of them, `e0c3b69`.
⟜ a missing cursor meant silence, permanently: `inbox NAME` printed nothing and stayed
that way for every fresh name — the mail view delivers the whole history instead
(`7abef9d`).
⟜ `status`'s unread figure was the log span after the cursor, not the reader's mail —
it is now the mail count, what `inbox` would print, `fcce8b0`.
⟜ `say --thread <id>` recorded a parent that does not exist, failing only when something
read that thread — refused at write time with `🔴 say: no post with id <id> — threads
attach to an existing post`, `f7928dc`. The raw JSON path keeps the log's tolerance by
design: explicit JSON is the caller's own statement.
