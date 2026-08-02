{-# LANGUAGE PatternSynonyms #-}

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
module Free.Agent.Diagram
  ( agentDiagram,
    diagramStep,
    diagramSteps,
    liftProcess,
    mooreBody,
    mooreProcess,
  )
where

import Circuit.Poly (Mono, System)
import Circuit.Poly.Process (runSystem, systemAsLens)
import Circuit.Poly.StringDiagram (Diagram, box, runDiagram)
import Circuit.Process (Process, pattern P, register)

-- | A monomial system as a one-box diagram.
--
-- @box . systemAsLens@: forward wire @s → o@ (state to output), backward
-- wire @i → s@ (input direction to next state).
agentDiagram :: System (->) s (Mono i o) -> Diagram s s o i
agentDiagram = box . systemAsLens

-- | One Moore step as a diagram run: @(next state, output at current
-- state)@.  Definitionally @(snd (runSystem sys s) i, fst (runSystem
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
liftProcess f = P id (const id) f

-- | The stateless body of a system as a 'Process': consume the input
-- (state transition), then read the output of the new state, and emit the
-- new state on the feedback wire.  This matches 'iterateSystem' /
-- 'systemAsProcess' semantics: the output is read /after/ consuming the
-- input.
mooreBody :: System (->) s (Mono i o) -> Process (i, s) (o, s)
mooreBody sys = liftProcess (\(i, s) -> let s' = snd (runSystem sys s) i in (fst (runSystem sys s'), s'))

-- | A system as a 'Process': the stateless 'mooreBody' with its state wire
-- bent back through a one-tick 'delay' — 'register' makes the delay
-- explicit in the wiring rather than implicit in a lazy knot.
--
-- Oracle-pinned: @scan (mooreProcess sys s0) is == iterateSystem sys s0 is@.
mooreProcess :: System (->) s (Mono i o) -> s -> Process i o
mooreProcess sys s0 = register s0 (mooreBody sys)
