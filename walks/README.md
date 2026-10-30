# walks

Behaviour walks for the bus CLI and the deck UI. The runner has been this UI's only
test suite and the wounds found the gaps; these are for development to run instead.

    walks/run.sh              the committed suite
    walks/run.sh --all        everything, dirty files included
    walks/run.sh 'cli-*'      one family
    bash walks/cli-inbox-flow.sh

`walks/run.sh` runs the **committed** suite by default: a walk whose file, or
`lib.sh` itself, differs from `HEAD` is listed and skipped, because a red tally should
mean the handed-in state failed rather than that an author is mid-edit. `--all` runs the
dirty files too, which is what an author wants while writing one.

Each walk sets up its own scratch root under `~/.hermes/cache/scratch/deck-walks/`,
seeds it, invokes one surface, and prints expected versus actual per step. PASS/FAIL,
non-zero exit on failure.

Scratch only: never the runner's live root, live daemon or frames. CLI walks need no
daemon at all. Deck walks load the deck in a scratch Emacs against a scratch root.

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

## the deck walks

Each batch deck walk loads the deck in a fresh `emacs -Q -batch` against its own scratch
root. `deck-lib` is their shared helper and has no `.sh` suffix, so the runner does not
count it as a walk.

⟜ `deck-send-failure.sh` ⟜ a CLI exit 3 keeps the composition intact, shows the exit
and CLI output in the error buffer, and appends no log bytes.
⟜ `deck-filter.sh` ⟜ audience filtering shows matching and public posts, and clearing
the filter restores every post.
⟜ `deck-depth.sh` ⟜ log, inbox and thread views cycle 1 → 2 → 0 → 1; both `TAB` and
`<tab>` resolve to the cycling command in each mode map.
⟜ `deck-inbox.sh` ⟜ a fresh inbox reads from the first post without creating a cursor;
`m` marks all printed mail read, and later mail remains unread.
⟜ `deck-quit.sh` ⟜ view `q` buries without killing the buffer; status `q` kills the
buffer and releases its file watch and repaint timer.
⟜ `deck-frame.sh` ⟜ a uniquely named scratch daemon loads installed Evil, not Doom's
init, and opens one GUI (`ns`) and one pseudo-terminal frame. Both show the three-window
deck with focus in status; `<tab>` and `q` reach the deck in Evil normal state. The
daemon and both clients are killed and asserted gone.

Batch proves commands, buffer state and mode-map bindings. The frame walk measures
window/focus state and keymap resolution in real frames. It does not inject OS keyboard
events or load the runner's Doom configuration, so physical key delivery and
Doom-specific extra maps remain unproven.

## the knowns, as of 31 Oct

Wrong behaviour that the walks assert, so a fix turns them red instead of passing
quietly. Two survive, and both were measured by the walk that keeps them.

⟜ **terminal `TAB` is intercepted by Evil** ⟜ in a real terminal frame with Evil normal
state, `TAB`/`C-i` resolves to `evil-jump-forward`, not `free-agent-bus-cycle-depth`;
`<tab>` and `q` still reach the deck. GUI `TAB`/`C-i` also follows Evil, while GUI
`<tab>` reaches the deck. `deck-frame.sh` records it; physical OS key injection and
Doom-specific extra maps are unproven and the walk says so.
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
