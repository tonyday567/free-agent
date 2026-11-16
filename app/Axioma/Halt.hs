{-# LANGUAGE OverloadedStrings #-}

-- | The halt form (Y1): which posts a seat is controlled by.
--
-- Born from a measured death. The seat @yin@ was retired twice on
-- @~/yin-bus@ on 30 Oct 2026 by status reports that led with the Landed
-- glyph — post 215 at 04:42:21Z and post 223 at 05:21:04Z, both from
-- @yang@, both reading @🟢 tranche N done …@. The grammar reads a mark on
-- both ('markOf'); the control form reads a label ('controlMark'). The
-- loop judges by the form, so a report wakes the seat and only a glyph
-- alone on its first line stops it.
module Axioma.Halt
  ( haltTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Post (Post (..), body)
import Circuit.Shared (stamped)
import Circuit.Agent.Mark (Mark (..), controlMark, markGlyph, markOf)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File (readCursor, writeCursor)
import Free.Agent.Delivery (Turn (..), TurnMetrics (..), TurnResult (..), newDeliveryGrant)
import System.Directory (doesDirectoryExist, removePathForcibly)
import System.Timeout (timeout)

-- | The first line of post 215 on ~/yin-bus, 2026-10-30T04:42:21Z.
yin215 :: Text
yin215 = "🟢 tranche 1 done. rev 580087c619b10ecfd9fbf1fccd4e87a7a6a3d311 (coffee, main). 11 files, +359."

-- | The first line of post 223 on ~/yin-bus, 2026-10-30T05:21:04Z.
--
-- The first line is the whole envelope, so the first line is the case.
yin223 :: Text
yin223 = "🟢 tranche 4 done. loom/bus-postmortem.md at a7189b9418ede2a640b322dafde796c993461019 (coffee, main). 247 lines, every claim citing post ids."

haltTopic :: IO ()
haltTopic = do
  putStrLn "the halt form (Y1)"
  assert "Y1a both death cases carry the grammar but not the control form" $
    (markOf (mkPost "yang" ["yin"] yin215) == Just Landed)
      && (controlMark (mkPost "yang" ["yin"] yin215) == Nothing)
      && (markOf (mkPost "yang" ["yin"] yin223) == Just Landed)
      && (controlMark (mkPost "yang" ["yin"] yin223) == Nothing)
  assert "Y1b a bare glyph stops the seat, whatever the author" $
    controlMark (mkPost "runner" ["yin"] (markGlyph Landed)) == Just Landed
      && controlMark (mkPost "yang" ["yin"] (markGlyph StandDown)) == Just StandDown
  assert "Y1c a bare first line is control, and its reason rides below it" $
    controlMark (mkPost "runner" ["yin"] (markGlyph StandDown <> "\nautomatic refresh at 123 prompt tokens"))
      == Just StandDown
  assert "Y1d the loop's own failure relay is not a stop order" $
    controlMark (mkPost "board-grok" ["yung"] (markGlyph Escalate <> " handler failed: user error"))
      == Nothing
  assert "Y1e a mark inside the body is not an envelope" $
    controlMark (mkPost "yang" ["yin"] ("status: " <> markGlyph Landed <> " landed")) == Nothing
  -- The loop: two death-case bodies in one drained backlog, then the control
  -- form. Under the prefix rule the seat halts on the first post and answers
  -- nothing, which is the measured death; here both reports reach one turn
  -- and the seat then stops.
  root <- tempRoot "free-agent-halt-form"
  writeCursor root "yin" 0
  _ <- postLocal root (mkPost "yang" ["yin"] yin215)
  _ <- postLocal root (mkPost "yang" ["yin"] yin223)
  _ <- postLocal root (mkPost "runner" ["yin"] (markGlyph Landed))
  seen <- newIORef ([] :: [[Text]])
  done <- newEmptyMVar
  grant <- newDeliveryGrant 100
  let handler storedBatch = do
        modifyIORef' seen (map (body . stamped) storedBatch :)
        pure
          Turn
            { awaitTurn = pure (TurnResponse []),
              cancelTurn = pure (),
              turnMetrics = pure (TurnMetrics False False 0)
            }
  _ <- forkIO (runAgentLoop "yin" ["yin"] root Nothing grant handler >> putMVar done ())
  exited <- timeout 30000000 (takeMVar done)
  answered <- readIORef seen
  cursor <- readCursor root "yin"
  assert "Y1f both status reports reach the seat in one batch" $
    answered == [[yin215, yin223]]
  assert "Y1g the seat stopped on the control form, cursor past all three" $
    isJust exited && cursor == 3
  exists <- doesDirectoryExist root
  unless (not exists) (removePathForcibly root)
