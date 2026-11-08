# fare ⟜ the restricted tool menu

fare is a stand-alone executable in this package (`app/fare.hs`) — a curated, bounded menu of the agent activities the week's audit actually measured (the evidence base: `~/storage/coffee-cards/token-audit.md` — discovery is the costly move, ~40% of spend). The same binary is the dsh local seat's tool surface (wave 3, full story in `~/storage/coffee-cards/fare.md`). Its job: keep tool results small so contexts stay cheap, on cloud models and local ones.

Live commands: `search`, `read`, `gate`, `state`, `index` (a decayed-area query over kimi wires — `fare index KEYWORD [ROOT] [--cutoff N]`, evidence boundary in `fare index --help`). `index` landed in source on 6 Nov and was installed the same day, so a seat on this box reaches it; a box still carrying the older binary returns `Invalid argument`, and the repair there is a rebuild rather than a flag. In the wings (unbuilt): `usage`, `pc`, `bash`.

Exact flags per command: `fare --help` and `fare <cmd> --help` are the source of truth; this file keeps what help does not carry.

## when to reach for it

⟜ code discovery ⟜ `fare search`, then `fare read` — locate, then window
⟜ repo state ⟜ `fare state` — dirties, recent commits, diffstat
⟜ build verdict ⟜ `fare gate` — build+ormolu+hlint+docspec lane, one line
⟜ the PC ⟜ not built: installed help has no `pc`. The lane today is plain non-interactive `ssh`; the designed `fare pc CMD` would hold that environment (the PATH trap, 94 paid wrappers in the audit)
⟜ anything else ⟜ not built: installed help has no `bash` and no `usage`. The lane today is `fare search` / `fare read` plus ordinary shell; the designed `fare bash` is the free-form meter where new practices enter and solidify into menu items

## the doctrine (load-bearing — read before first use)

⟜ **verdicts first** ⟜ the first line of every output is the answer and the next move. Read it first; it usually suffices.
⟜ **layered detail** ⟜ the distilled answer is the interface; detail is one ask away, not in the response. Truncations write a full artifact to a temp file and print `next: fare read X 1,40` — page deeper only when needed.
⟜ **bounded by construction** ⟜ layer-1 answers sit under the item's own bound (2000-byte item bound for search/read; state's body). No byte-budget flags in steady state.
⟜ **exact lines as evidence** ⟜ outputs are path:line:content — quote them verbatim; they compose (search's line numbers are read's ranges).
⟜ **"0 matches" is an answer** ⟜ search exits 0 on no matches; a failed hunt is not an error.

## the menu, beyond the flags

⟜ `fare search` ⟜ substring, case-sensitive, no regex — on purpose (the audit's greps were definition hunts). Skips `.git`/`dist-newstyle`/`dist`/`.stack-work`/`node_modules`, symlinks, non-UTF-8, FIFOs.
⟜ `fare read` ⟜ never whole files. Windows clamp; start past EOF reports honestly (exit 0); bad range / missing file / binary → stderr, exit 1.
⟜ `fare gate` ⟜ short-circuits; exit 1 on FAIL. ormolu uses the house `git ls-files` form — untracked `.hs` is not checked. `--docspec` / `--docspec-full` test the last build. The default lane comes from `ROOT/.fare` (`gate.docspec = none|repository|full`); this checkout selects `repository`. See the Development section of [readme.md](readme.md) for the configured behaviour.
⟜ `fare state` ⟜ header verdict (branch, upstream position, last commit, dirty count — never sliced) + bounded body. Full diffs are not its job and `fare bash` is unbuilt: use `git diff` / `git show` today, and `fare read` to window a diff artifact.
⟜ in the wings ⟜ `fare usage` (the meter), `fare pc`, `fare bash` (unbuilt; optparse forwards dash-leading args only after `--`, e.g. `fare bash git status -- --short`)

## the seat contract (dsh seats, from wave 3)

dsh's bash tool requires a `description` on every call; `justification` is only valid with `sandbox_permissions` and gets the call rejected. The shape that always works: `{command, description}`. Seat prompts should carry this contract — left to be discovered it cost a 12b seat 5 of 7 calls; documented, zero misses. Full story: `~/storage/coffee-cards/fare.md` run 3.

## traps

⟜ `--head` / `--tail` is a testing knob (untried seats and harnessings), not steady state — the item's own layers pick the end by default
⟜ `fare read` has no `-N` tail-range syntax — optparse eats dash-leading arguments; use `--tail`
⟜ the Mac and PC copies drift with their free-agent checkouts — after building new fare items, `cabal install exe:fare` again

## links

⟜ `~/storage/coffee-cards/fare.md` ⟜ the full card: design principles, wave-3 harness journal, run-2 correction, run 3, latency introspection
⟜ `~/storage/coffee-cards/token-audit.md` ⟜ the evidence base
⟜ [../../coffee/buff/local-agent.md](../../coffee/buff/local-agent.md) ⟜ the PC/dsh harness fare rides
⟜ [seats.md](seats.md) ⟜ the free-agent seat runbook
