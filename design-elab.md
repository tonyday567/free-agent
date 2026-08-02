# design ⟜ surfacing primitives · elab

**claim** circuits names primitives that coders already use — trace, channel,
state, parallel — and leaves them open as inspectable structure rather than
sealing them inside runtimes.

  **elab** The library is a taxonomy. Each function — retry, fork, par, ambient —
  gets its proper spot. The post writes itself: explain famous functions, each
  in its place. `retry` is the jewel in the crown. No theory-first framing; the
  primitives ARE the library.

**claim** build in the free algebra, discharge with `run`.

  **elab** Three free constructions: `Loop` (free feedback — readable,
  pattern-matchable, transformable), `Poly` (free state — Moore machines as
  polynomial functors), `Ends` (free channels — companion/conjoint, split and
  pluggable). Structure stays inspectable until you choose to close it. `run`
  is a single call at the boundary.

**claim** polymorphic in tensor and arrow — no privileged path.

  **elab** `Loop t arr a b` works for lazy knot or iteration, pure or Kleisli,
  same combinator. `System arr s p` works for any arrow, any state, polynomial
  interface. `Ends arr a b` works for any arrow, companion/conjoint split.
  Neutral by construction.

**claim** every operation you can open, you can close.

  **elab** Three pairs: `strength` opens a loop, `trace` closes it. `open`
  splits channel ends, `close` plugs them together. `encode` seals structure
  into computation, `run` discharges it. These are adjunctions (elaboration,
  not the lead).

**claim** axioma: laws that compile, green lights that gate the build.

  **elab** Every package has an `axioma.hs` — exact oracles, property tests.
  Admission question: does this domain come with a primary source whose axioms
  compile, with exact answers to check against?

**claim** machina: measurement that diagnoses, not pass/fail.

  **elab** Three instruments: timer (that), Core (how), allocation (why). Gap
  classification: wrong order? compiler ceiling? code residue? Machina packages
  in `~/machina/` — can pull in anything. Substrate core stays thin.

**claim** substrate is the integration canary.

  **elab** `~/haskell/substrate/` depends on every package.
  `substrate-green-lights` proves everything compiles and links.
  `~/haskell/` holds core + axioma. `~/machina/` holds measurement. Clean
  dependency split.

**claim** house style: free over fixed, polymorphic over privileged, concrete-first.

  **elab** Free constructions over fixed representations. Polymorphic in tensor
  and arrow. Defaults-first — merge concerns over proliferating APIs. Short
  plain names over jargon. Concrete-first, then generalise. Metric: how small a
  change with confidence?
