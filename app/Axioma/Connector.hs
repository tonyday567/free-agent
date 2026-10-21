{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | connector turn grammar (B7): zero/one/two-frame turns.
module Axioma.Connector
  ( connectorTopic,
  )
where

import Axioma.Common (assert, mkPost)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (pattern Stamped)
import Circuit.Agent.StdPorts (Frame (..), ProcEnds (..))
import Circuit.Machine (polesK)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Free.Agent.Connector (ConnectorConfig (..), defaultConnectorConfig, runTurn)

connectorTopic :: IO ()
connectorTopic = do
  -------------------------------------------------------------------------
  -- Connector turn grammar (B7)
  --
  -- Process stdout is a shared bus with no envelopes.  The connector acts as
  -- the stamping office: it commits one command, reads the response frame,
  -- and reposts it with 'to' set to the asker and 'thread' set to the ask id.
  -- Zero-frame and two-frame violations are detected in-band by the same
  -- stamp grammar.
  -------------------------------------------------------------------------
  putStrLn "connector turn grammar"
  do
    let mkAsk pid asker addrs bodyText = do
          now <- getCurrentTime
          pure (Stamped (now, pid) (mkPost asker addrs bodyText))
        testCfg =
          (defaultConnectorConfig "connector")
            { connCommandTimeout = 100_000,
              connExtraFrameTimeout = 100_000
            }
        mockProcEnds frames = do
          cmdQ <- newTQueueIO
          respQ <- newTQueueIO
          let stdio = polesK (atomically . writeTQueue cmdQ) (atomically $ readTQueue respQ)
              stderrE = polesK (\_ -> pure ()) (pure (Payload ""))
              ends = ProcEnds stdio stderrE (pure ())
          pump <- async $ do
            _ <- atomically $ readTQueue cmdQ
            traverse_ (atomically . writeTQueue respQ . Payload) frames
          pure (ends, pump)

    -- One frame: the response carries the ask id in its thread.
    do
      ask <- mkAsk 0 "human" ["connector"] "hello"
      (repl, pump) <- mockProcEnds ["ack: 0"]
      replies <- runTurn testCfg repl ask
      cancel pump
      assert "one-frame turn reposts with asker as 'to' and ask id as thread" $
        case replies of
          [r] ->
            body r == "ack: 0"
              && to r == ["human"]
              && thread r == [0]
          _ -> False

    -- Zero frames: no response arrives, so the connector emits a diagnostic
    -- post stamped with the ask id.
    do
      ask <- mkAsk 1 "human" ["connector"] "hello"
      (repl, pump) <- mockProcEnds []
      replies <- runTurn testCfg repl ask
      cancel pump
      assert "zero-frame turn emits a diagnostic stamped with the ask id" $
        case replies of
          [r] ->
            "<zero-frame>" `T.isInfixOf` body r
              && to r == ["human"]
              && thread r == [1]
          _ -> False

    -- Two frames: both frames are stamped with the same ask id, and the
    -- extra frame is marked as a grammar violation.
    do
      ask <- mkAsk 2 "human" ["connector"] "hello"
      (repl, pump) <- mockProcEnds ["ack: 2", "spurious"]
      replies <- runTurn testCfg repl ask
      cancel pump
      assert "two-frame turn detects the extra frame and stamps both with the ask id" $
        case replies of
          [r1, r2] ->
            body r1 == "ack: 2"
              && to r1 == ["human"]
              && thread r1 == [2]
              && "<extra-frame>" `T.isInfixOf` body r2
              && to r2 == ["human"]
              && thread r2 == [2]
          _ -> False
