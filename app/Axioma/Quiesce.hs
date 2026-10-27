{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | tailLog draining + the quiescence Moore (Q1-Q5).
module Axioma.Quiesce
  ( quiesceTopic,
  )
where

import Axioma.Common (assert, iterateAgent, mkPost, tempRoot)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (frameStored, parseTimeText, stamped, pattern Stamped)
import Circuit.Agent.Mark (Mark (..), markGlyph, markOf)
import Control.Concurrent (forkIO, killThread, modifyMVar_, newMVar, readMVar, threadDelay)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File (Decision (..), Flow (..), QEvent (..), quiesceAgent, tailLog)
import System.FilePath ((</>))

quiesceTopic :: IO ()
quiesceTopic = do
  -------------------------------------------------------------------------
  -- tailLog: offset draining, partial trailing line, halt mid-batch.
  --
  -- tailLog reads complete lines only; a writer mid-append is left for the
  -- next drain. A callback that returns 'Halt' stops further delivery.
  -------------------------------------------------------------------------
  putStrLn "tailLog"
  do
    root <- tempRoot "free-agent-tail-axioma"
    let path = root </> "log.jsonl"
        mkStored i ts f t b = case parseTimeText ts of
          Just ts' -> Stamped (ts', i) (Post f Nothing t [] b)
          Nothing -> error $ "bad timestamp: " <> show ts
        frame i ts f t b = frameStored @Text (mkStored i ts f t b)
    -- Write one complete line and one partial line (no trailing newline).
    TIO.writeFile
      path
      ( frame 0 "2026-08-05T00:00:00" "human" ["test"] "one"
          <> "\n"
          <> frame 1 "2026-08-05T00:00:01" "human" ["test"] "two"
      )
    collected <- newMVar []
    tid <-
      forkIO $
        tailLog path ["test"] 0 Nothing $ \stored -> do
          modifyMVar_ collected (pure . (stored :))
          pure (if body (stamped stored) == "halt" then Halt else Continue)
    -- Wait for the initial drain to finish before appending.
    threadDelay 200000
    initial <- readMVar collected
    assert "offset drain: the complete line is delivered, the partial line is not" $
      map (body . stamped) (reverse initial) == ["one"]
    -- Complete the partial line and append a halt plus one more post.
    TIO.appendFile
      path
      ( "\n"
          <> frame 2 "2026-08-05T00:00:02" "human" ["test"] "halt"
          <> "\n"
          <> frame 3 "2026-08-05T00:00:03" "human" ["test"] "four"
          <> "\n"
      )
    -- Wait for fsnotify to wake the drain and for the halt to stop the loop.
    threadDelay 1000000
    killThread tid
    final <- readMVar collected
    let bodies = map (body . stamped) (reverse final)
    assert "partial trailing line is delivered once it is completed" $
      bodies == ["one", "two", "halt"]
    assert "halt mid-batch stops delivery" $
      "four" `notElem` bodies

  -------------------------------------------------------------------------
  -- quiesceAgent: the quiescence Moore (Q1-Q5)
  --
  -- Pure event-sequence tests over the machine; no wall clock in the
  -- room. Tick = an empty cycle ends with nothing addressed delivered;
  -- Delivered = an addressed post drained; Activity = chatter to other
  -- seats. The verdict must carry ⌛, not 🔵 — the timeout-inference
  -- laundering oracle.
  -------------------------------------------------------------------------
  putStrLn "quiesceAgent"
  do
    let n = 3
        sys = quiesceAgent n
        run = iterateAgent sys 0
        outsN = run (replicate n Tick)
    -- Q1: a silent bus stands down after n consecutive empty cycles;
    -- n-1 cycles must not. Catches a machine that counts the startup
    -- drain as an implicit tick (the old busy-gate) — that one fires
    -- at n-1.
    let outsNm1 = run (replicate (n - 1) Tick)
    assert "Q1 silent bus: Tick × n stands down; Tick × (n-1) does not" $
      isJust (outsN !! (n - 1)) && isNothing (outsNm1 !! (n - 2))
    -- Q2: chatter neither stalls nor resets the count, and is never a
    -- stand-down by itself. Catches the old reset-on-any-signal
    -- semantics, which starved under a chatty bus.
    assert "Q2 chatter: Activity between ticks neither stalls nor resets" $
      isJust (last (run [Tick, Activity, Tick, Activity, Tick]))
    assert "Q2 chatter: Activity alone never stands down" $
      all isNothing (run (replicate (n + 1) Activity))
    -- Q3: a delivery since the last tick window resets the count, and
    -- the count re-arms. Catches a Delivered-as-no-op machine, which
    -- stands down at the final tick of the first sequence.
    assert "Q3 delivered resets: a delivery blocks stand-down" $
      all isNothing (run [Tick, Tick, Delivered, Tick, Tick])
    let rearmed = run [Tick, Tick, Delivered, Tick, Tick, Tick]
    assert "Q3 delivered resets: the count re-arms and stands down again" $
      isNothing (rearmed !! 4) && isJust (rearmed !! 5)
    -- Q4: a delivery before the first tick is honored — an initial
    -- state that dropped it would stand down after n-1 further ticks.
    assert "Q4 a delivery before the first tick is honored" $
      all isNothing (run (Delivered : replicate (n - 1) Tick))
    let afterDelivered = run (Delivered : replicate n Tick)
    assert "Q4 after a delivery, n fresh ticks stand down" $
      all isNothing (init afterDelivered) && isJust (last afterDelivered)
    -- Q5: the verdict carries the Timeout mark — markOf sees ⌛, not 🔵.
    -- Catches the laundered-🔵 regression exactly: a machine whose body
    -- starts with the StandDown glyph fails the first clause.
    case outsN !! (n - 1) of
      Just d -> do
        assert "Q5 the verdict carries Timeout, not StandDown" $
          markOf (mkPost "seat" ["boss"] (decisionBody d)) == Just Timeout
            && markOf (mkPost "seat" ["boss"] (decisionBody d)) /= Just StandDown
        assert "Q5 the verdict names the cycle count" $
          decisionBody d == markGlyph Timeout <> " standing down after 3 empty cycles"
      Nothing -> assert "Q5 a verdict was reached" False
