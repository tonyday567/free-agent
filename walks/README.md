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

## the two assertions

⟜ `assert_eq` ⟜ the behaviour we want. A failure is a bug to fix.
⟜ `assert_known` ⟜ behaviour that is wrong and currently true, written down with its
  reason. It passes while the behaviour holds and fails the moment it changes, so
  fixing a defect turns the walk red and the assertion gets promoted to `assert_eq`.

## the walks

⟜ `cli-inbox-flow.sh` ⟜ unread is id >= cursor, `--mark-read` sets the cursor to the
highest printed id plus one, a caught-up name prints nothing and exits 0.
⟜ `cli-inbox-unset-cursor.sh` ⟜ the wound behind the runner's inbox reading zero: a
name with mail and no cursor file receives nothing, silently, exit 0. Three knowns.
⟜ `cli-say-addressing.sh` ⟜ who a post is for: `--to NAME` delivers; an omitted `--to`
posts publicly rather than being refused; `--to ""` writes a recipient named "";
ancestry is carried verbatim, including dangling ids, and the read of a dangling id is
where it fails loudly. Three knowns.
⟜ `cli-retire.sh` ⟜ a retired seat leaves the live roster and keeps its cursor, mail
and log; no process is signalled.
⟜ `cli-watch.sh` ⟜ a watcher skips pre-watch history, prints only new addressed posts
as raw JSON frames, creates no cursor, keeps waiting after delivery, and leaves no
process when killed. `watch` has no `--depth` option.

## the deck walks

Each `deck-*.sh` loads the deck in a fresh `emacs -Q -batch` against its own scratch
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

Batch proves commands, buffer state and mode-map bindings. Visible window layout and
focus, actual GUI/terminal key delivery, and Doom's evil auxiliary maps need a real
configured frame; these walks do not claim them.

## the knowns, as of 31 Oct

Wrong behaviour that the walks assert, so a fix turns them red instead of passing
quietly. shen's two verification walks (`cli-verify-inbox-cursor`,
`cli-verify-addressing-reach`, `09e2b03`) added two of these and corrected my reading of
a third.

⟜ **an absent cursor reads as caught up, permanently** ⟜ `inbox NAME` with no cursor file
prints nothing and exits 0, and stays that way: the default is the log's latest id,
recomputed on every call, so mail posted later never appears and `--mark-read` cannot
bootstrap it. The deck's fresh-inbox path was fixed to read from the first post
(`1f78f6a`); the CLI's `inbox` was not. It is the resting state of every fresh name,
which is what makes it the most dangerous of the set.
⟜ **a missing `--to` is not refused** ⟜ it writes `to: []`, and `--to ""` writes
`to: [""]`. Both are delivered to nobody: `deliversTo` answers False for each, and
`read NAME` and `inbox NAME` show only genuinely addressed posts. "Public" is a property
of the log view, not a delivery state.
⟜ **`--thread` accepts an id that does not exist** ⟜ hidden in the log until something
reads that thread, at which point `read --thread <id>` fails loudly with `🔴 no post with
id <id>`. The late failure is the surprise, not the acceptance.
⟜ **`status`'s unread figure is not the inbox's** ⟜ behind is counted as
`maxId − cursor + 1`, so a name with one addressed post reports "3 unread" while its
inbox prints one line.
⟜ **`read` on a root with no `log.jsonl` exits 0 in silence** ⟜ `runRead` skips the
missing file and never calls `requireBus`, unlike `say` and `inbox`, which refuse with the
named message.
⟜ **`read --cursor NAME` prints nothing for a fresh name** ⟜ the same log-tip default as
the first row, reached through a different door.
