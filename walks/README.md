# walks

Behaviour walks for the bus CLI and the deck UI. The runner has been this UI's only
test suite and the wounds found the gaps; these are for development to run instead.

    walks/run.sh              every walk
    walks/run.sh 'cli-*'      one family
    bash walks/cli-inbox-flow.sh

Each walk sets up its own scratch root under `~/.hermes/cache/scratch/deck-walks/`,
seeds it, invokes one surface, and prints expected versus actual per step. PASS/FAIL,
non-zero exit on failure.

Scratch only: never the runner's live root, live daemon or frames. CLI walks need no
daemon at all. Deck walks load the deck in a scratch Emacs against a scratch root.

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

## the knowns, as of 31 Oct

⟜ an absent cursor reads as caught up, not as everything-new (CLI `inbox`; the deck's
fresh-inbox path was fixed at `1f78f6a` and the CLI was not).
⟜ a missing `--to` is accepted as a public post, and `--to ""` produces `to: [""]` —
mail addressed to nobody, which no surface will ever deliver.
⟜ `--thread` accepts an id that does not exist; the failure surfaces later, at the read.
