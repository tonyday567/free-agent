{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Bridge: agents as string diagrams (stage 1 of endgame-path).
--
-- A monomial 'System' is a lens from its state interface
-- (@systemAsLens@: @System s p ≅ Poly(S y^S, p)@), and 'box' lifts such a
-- lens into the string-diagram DSL.  The result runs one Moore step per
-- 'runDiagram' call: the forward wire carries state → output, the backward
-- wire carries input-direction → next state.
--
-- Stage 1b: the bend, at the 'Process' layer.  A stateful agent decomposes
-- as a stateless body plus cross-tick feedback: 'register' closes the
-- state wire with 'delay' making the one-tick lag observable (sound for
-- strict state, where the lazy cartesian knot would diverge).  So:
--
-- @
-- agent = box + bend + delay ≡ mooreProcess sys s0 = register s0 body
-- @
--
-- What is still missing is the same wiring inside the
-- 'Circuit.Poly.StringDiagram' surface itself (a delay box and a trace
-- combinator over a 'Process' base); the oracles pin the semantics the
-- surface will need to reproduce.
--
-- Stage 2: 'meetingSkeleton' reads a conversation back as a drawing —
-- the unification claim that the log records the wiring.  Stage 2b draws
-- the full post-DAG: forks and syntheses are visible copy/merge spiders,
-- routed with 'SSwap'-built permutations.
module Free.Agent.Diagram
  ( agentDiagram,
    diagramStep,
    diagramSteps,
    liftProcess,
    mooreBody,
    mooreProcess,
    meetingSkeleton,
    skeletonLabels,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Poly (Mono, System)
import Circuit.Poly.StringDiagram (Diagram, SDiagram (..), box, runDiagram)
import Circuit.Process (Process (..), register)
import Circuit.System (runSystemMono, systemAsLens)
import Data.List (delete, elemIndex, foldl', mapAccumL)
import Data.Text qualified as T

-- | A monomial system as a one-box diagram.
--
-- @box . systemAsLens@: forward wire @s → o@ (state to output), backward
-- wire @i → s@ (input direction to next state).
agentDiagram :: System (->) s (Mono i o) -> Diagram s s o i
agentDiagram = box . systemAsLens

-- | One Moore step as a diagram run: @(next state, output at current
-- state)@.  Definitionally @(snd (runSystemMono sys s) i, fst (runSystemMono
-- sys s))@ — the oracle pins exactly this.
diagramStep :: System (->) s (Mono i o) -> s -> i -> (s, o)
diagramStep sys s i = runDiagram (agentDiagram sys) (s, i)

-- | Iterate a system over inputs through the diagram, mirroring
-- 'iterateSystem' (which emits the output of the state /after/ each
-- transition).  Each step runs the diagram twice with the same input: once
-- to consume (state transition), once to observe (output at the new
-- state).  The backward pass is pure, so the second run is harmless — and
-- both passes go through 'runDiagram', so the oracle exercises the bridge
-- end to end.
diagramSteps :: System (->) s (Mono i o) -> s -> [i] -> [o]
diagramSteps _ _ [] = []
diagramSteps sys s (i : is) =
  let (s', _) = diagramStep sys s i
      (_, o) = diagramStep sys s' i
   in o : diagramSteps sys s' is

-- | Lift a pure function to a stateless 'Process' (the box a bend closes).
liftProcess :: (a -> b) -> Process a b
liftProcess f = Process id (const id) f

-- | The stateless body of a system as a 'Process': consume the input
-- (state transition), then read the output of the new state, and emit the
-- new state on the feedback wire.  This matches 'iterateSystem' /
-- 'systemAsProcess' semantics: the output is read /after/ consuming the
-- input.
mooreBody :: System (->) s (Mono i o) -> Process (i, s) (o, s)
mooreBody sys = liftProcess (\(i, s) -> let s' = snd (runSystemMono sys s) i in (fst (runSystemMono sys s'), s'))

-- | A system as a 'Process': the stateless 'mooreBody' with its state wire
-- bent back through a one-tick 'delay' — 'register' makes the delay
-- explicit in the wiring rather than implicit in a lazy knot.
--
-- Oracle-pinned: @scan (mooreProcess sys s0) is == iterateSystem sys s0 is@.
mooreProcess :: System (->) s (Mono i o) -> s -> Process i o
mooreProcess sys s0 = register s0 (mooreBody sys)

-- | The drawing skeleton of a conversation: each post is a box labelled by
-- its sender, wired by its 'thread' ancestry — "the log is the diagram of
-- the meeting that produced it".  A root is @SBox label 0 1@ (nothing
-- feeds it), a reply @SBox label 1 1@, and a synthesis with m parents is
-- preceded by a visible merge spider ('SSpider' @m 1@); a post cited as
-- parent by k > 1 later posts forks its output through a visible copy
-- spider ('SSpider' @1 k@).
--
-- One pass over the log, oldest first: the state is the list of live wire
-- ends (each tagged by the thread edge it feeds, or by the post whose
-- uncited output it carries to the boundary), and each post emits one
-- layer — permute the parent wires to the back (adjacent 'SSwap's), merge,
-- box, fork.  A dangling thread edge (parent not in the log) becomes a
-- free input wire, present from the left boundary.
meetingSkeleton :: [Post a] -> SDiagram
meetingSkeleton [] = SWire
meetingSkeleton ps = foldr1 SThenD (snd (mapAccumL step dangling [0 .. length ps - 1]))
  where
    -- thread edges are exact positional ids into the oldest-first log;
    -- 'Nothing' is a dangling id (expected not to happen for a well-formed
    -- stamped meeting).
    sources =
      [ [if i < fromIntegral j then Just i else Nothing | i <- thread p]
      | (j, p) <- zip ([0 ..] :: [Int]) ps
      ]
    -- the citation edges of post j, in consumer (log) order
    cited j = [(i, pos) | (i, ss) <- zip [0 ..] sources, (pos, Just j') <- zip [0 ..] ss, j' == fromIntegral j]
    -- one free input wire per dangling edge
    dangling = [Right (i, pos) | (i, ss) <- zip [0 ..] sources, (pos, Nothing) <- zip [0 ..] ss]

    step :: [Either Int (Int, Int)] -> Int -> ([Either Int (Int, Int)], SDiagram)
    step live i = (rest ++ outs, layer)
      where
        label = T.unpack (from (ps !! i))
        m = length (thread (ps !! i))
        nW = length live
        -- bring the m parent wires to the back, in thread order, filling
        -- positions right to left so each bubble only crosses unfixed wires
        (swaps, ordered) =
          foldl' bubble ([], live) (zip [nW - 1, nW - 2 ..] (reverse (map (edge i) [0 .. m - 1])))
        bubble (ss, xs) (t, w) = case elemIndex w xs of
          Nothing -> error "meetingSkeleton: parent wire not live"
          Just p -> (ss ++ [p .. t - 1], take t (delete w xs) ++ [w] ++ drop t (delete w xs))
        rest = take (nW - m) ordered
        outs = case cited i of
          [] -> [Left i]
          es -> map (uncurry edge) es
        mergeBox = case m of
          0 -> SBox label 0 1
          1 -> SBox label 1 1
          _ -> SThenD (SSpider m 1) (SBox label 1 1)
        consumedPart
          | length outs > 1 = SThenD mergeBox (SSpider 1 (length outs))
          | otherwise = mergeBox
        body = case rest of
          [] -> consumedPart
          rs -> SBeside (wires (length rs)) consumedPart
        layer = case swaps of
          [] -> body
          ss -> SThenD (foldl1 SThenD (map (swapAt nW) ss)) body

    edge i pos = Right (i, pos)

    -- adjacent swap at index s on a bundle of n wires
    swapAt n s =
      foldr1
        SBeside
        ([wires s | s > 0] ++ [SSwap] ++ [wires (n - s - 2) | n - s - 2 > 0])

    -- identity on a bundle of r >= 1 wires
    wires 1 = SWire
    wires r = SBeside SWire (wires (r - 1))

-- | The box labels of a skeleton, in composition order.
skeletonLabels :: SDiagram -> [String]
skeletonLabels SWire = []
skeletonLabels (SBox l _ _) = [l]
skeletonLabels (SSpider _ _) = ["spider"]
skeletonLabels SPrismBox = ["prism"]
skeletonLabels (SBeside f g) = skeletonLabels f ++ skeletonLabels g
skeletonLabels (SThenD f g) = skeletonLabels f ++ skeletonLabels g
skeletonLabels SBend = ["cup"]
skeletonLabels SBend' = ["cap"]
skeletonLabels (STurn f) = skeletonLabels f
skeletonLabels SUnitL = ["unitL"]
skeletonLabels SUnitL' = ["unitL'"]
skeletonLabels SUnitR = ["unitR"]
skeletonLabels SUnitR' = ["unitR'"]
skeletonLabels SAssoc = ["assoc"]
skeletonLabels SAssoc' = ["assoc'"]
skeletonLabels SSwap = ["swap"]
skeletonLabels (STrace f) = skeletonLabels f
