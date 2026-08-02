-- | Bridge: agents as string diagrams (stage 1 of endgame-path).
--
-- A monomial 'System' is a lens from its state interface
-- (@systemAsLens@: @System s p ≅ Poly(S y^S, p)@), and 'box' lifts such a
-- lens into the string-diagram DSL.  The result runs one Moore step per
-- 'runDiagram' call: the forward wire carries state → output, the backward
-- wire carries input-direction → next state.
--
-- Feedback (state wires bent back through a delay, meeting self-loops) is
-- not expressible with the current 'Circuit.Poly.StringDiagram' surface:
-- there is no delay box, and a bare lazy knot would diverge without a
-- quiescence story.  That is the identified next step (stage 1b).
module Free.Agent.Diagram
  ( agentDiagram,
    diagramStep,
    diagramSteps,
  )
where

import Circuit.Poly (Mono, System)
import Circuit.Poly.Process (runSystem, systemAsLens)
import Circuit.Poly.StringDiagram (Diagram, box, runDiagram)

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
