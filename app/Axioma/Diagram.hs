{-# LANGUAGE OverloadedStrings #-}

-- | diagram bridge + bend with delay: machine bodies as string diagrams.
module Axioma.Diagram
  ( diagramTopic,
  )
where

import Axioma.Common (assert, iterateAgent, mkPost, runMono)
import Circuit.Agent (Post, tape)
import Circuit.Category (Category ((.)))
import Circuit.Machine (Body (Body), Moore, delay, register, scan)
import Circuit.Tensor (Const (..))
import Circuit.Traced (Strength (strength), Yank (yank))
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Diagram (diagramStep, diagramSteps, liftProcess, machineProcess)
import Prelude hiding ((.))

diagramTopic :: IO ()
diagramTopic = do
  -------------------------------------------------------------------------
  -- Diagram bridge (endgame stage 1): an agent is a box; one diagram run
  -- is one MachineP step.  Feedback wiring (bend + delay) is not yet
  -- expressible in the string-diagram surface — see Free.Agent.Diagram.
  -------------------------------------------------------------------------
  putStrLn "diagram bridge"
  do
    let sysN = Body (\i -> i) (\(s, i) -> s + i) (Const . (+ 1)) :: Moore (,) Int (->) Int Int
    assert "diagram step is the machine body's (put, get)" $
      diagramStep sysN 5 3 == (snd (runMono sysN 5) 3, fst (runMono sysN 5))
    assert "diagram steps mirror iterateAgent" $
      diagramSteps sysN 0 [1 .. 5] == iterateAgent sysN 0 [1 .. 5]

  do
    let echoObs =
          tape (\hist -> [mkPost "bot" ["human"] ("n:" <> T.pack (show (length hist)))]) ::
            Moore (,) [Post Text] (->) (Post Text) [Post Text]
        p0 = mkPost "human" ["bot"] "one"
        ins =
          [ p0,
            mkPost "human" ["bot"] "two",
            mkPost "human" ["bot"] "three"
          ]
    assert "tape agent: diagram step is the machine body's (put, get)" $
      diagramStep echoObs [] p0
        == (snd (runMono echoObs []) p0, fst (runMono echoObs []))
    assert "tape agent: diagram steps mirror iterateAgent" $
      diagramSteps echoObs [] ins == iterateAgent echoObs [] ins

  -------------------------------------------------------------------------
  -- Bend with delay (endgame stage 1b): a stateful agent decomposes as a
  -- stateless body plus cross-tick feedback; 'register' closes the state
  -- wire with the one-tick 'delay' explicit in the wiring.
  -------------------------------------------------------------------------
  putStrLn "bend with delay"
  do
    -- a standalone delay emits the initial value, then each input one
    -- tick late.
    -- (Inside register, where the feedback wire carries the body's own
    -- state output, this is exactly "observable one tick late".)
    assert "delay emits initial value, then each input one tick late" $
      scan (delay 0) [1, 2, 3, 4 :: Int] == [0, 1, 2, 3]

  do
    let sysN = Body (\i -> i) (\(s, i) -> s + i) (Const . (+ 1)) :: Moore (,) Int (->) Int Int
    assert "register body with delay mirrors iterateAgent" $
      scan (machineProcess sysN 0) [1 .. 5] == iterateAgent sysN 0 [1 .. 5]

  do
    let echoObs =
          tape (\hist -> [mkPost "bot" ["human"] ("n:" <> T.pack (show (length hist)))]) ::
            Moore (,) [Post Text] (->) (Post Text) [Post Text]
        ins = [mkPost "human" ["bot"] "one", mkPost "human" ["bot"] "two"]
    assert "tape agent: register body with delay mirrors iterateAgent" $
      scan (machineProcess echoObs []) ins == iterateAgent echoObs [] ins

  -- The bend-and-delay identity from Circuit.Machine: for bodies whose
  -- fixed point is independent of the initial feedback value, register's
  -- wiring equals the cartesian trace with delay on the feedback wire.
  do
    let body = liftProcess (\(i, _s) -> (i * 2, 0 :: Int))
        swapP = liftProcess (\(a, b) -> (b, a))
        ins = [1, 2, 3, 4 :: Int]
        viaRegister = scan (register 99 body) ins
        viaTrace = scan (yank (swapP . (body . strength (delay 99)) . swapP)) ins
    assert "register ≡ trace with delay on the feedback wire" $
      viaRegister == viaTrace && viaRegister == [2, 4, 6, 8]
