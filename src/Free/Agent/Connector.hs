{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistent REPL agent as a bus participant.
--
-- 'runConnector' spawns a process (via 'openProc'), attaches to the
-- free-agent bus, and loops: await addressed posts → commit body to repl
-- stdin → block on the next stdout frame → scribe reply.
--
-- Framing is port-side: the repl's prompt grammar ('procMarks') decides
-- turn boundaries, and the emit end blocks until a complete frame arrives.
-- No polling; the timeouts here are deadlines around blocking reads, not
-- backoff loops.
module Free.Agent.Connector
  ( ConnectorConfig (..),
    defaultConnectorConfig,
    runConnector,
  )
where

import Circuit.Agent (Name, Post (..), deliversTo, mkPost)
import Circuit.Agent.Framing (Stamped (..))
import Circuit.Agent.StdPorts
  ( ProcConfig (..),
    ProcEnds (..),
    defaultProcConfig,
    ghciMarks,
    openProc,
  )
import Circuit.Ends (Ends (..), HasUnit (..), commit, emit, open)
import Circuit.Layer (run)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (unless, void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Free.Agent.Bus (Bus, awaitSince, closeBus, openBus, scribeIO)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Prelude

data ConnectorConfig = ConnectorConfig
  { connName :: Name,
    connBusRoot :: FilePath,
    connRepl :: ProcConfig,
    -- | Deadline (microseconds) for the repl's first frame.
    connStartupTimeout :: Int,
    -- | Deadline (microseconds) for one command's reply frame.
    connCommandTimeout :: Int
  }

defaultConnectorConfig :: Name -> ConnectorConfig
defaultConnectorConfig name =
  ConnectorConfig
    { connName = name,
      connBusRoot = "/tmp/free-agent-bus",
      connRepl =
        defaultProcConfig
          { procCommand = "cabal",
            procArgs = ["repl"],
            procMarks = ghciMarks
          },
      connStartupTimeout = 180_000_000,
      connCommandTimeout = 60_000_000
    }

runConnector :: ConnectorConfig -> IO ()
runConnector cfg = do
  let name = connName cfg
  bus <- openBus (connBusRoot cfg)
  repl <- runKleisli (run (openProc encodeUtf8 decodeUtf8 (connRepl cfg))) ()
  void $ scribeIO bus (mkPost name [] ("starting repl: " <> T.pack (procCommand (connRepl cfg)) <> " " <> T.unwords (map T.pack (procArgs (connRepl cfg)))))

  -- Drain the initial frame (up to the first prompt).
  mFirst <- timeout (connStartupTimeout cfg) (emitText (procStdio repl))
  whenNothing mFirst $
    hPutStrLn stderr "connector: startup deadline expired before first prompt"

  err0 <- drainStderr (procStderr repl)
  unless (T.null err0) $
    hPutStrLn stderr ("connector stderr: " <> T.unpack (T.take 200 err0))

  -- Main loop.
  void $ scribeIO bus (mkPost name [] "repl ready")
  loop bus repl name 0
  closeBus bus
  procClose repl
  where
    whenNothing m act = maybe act (const (pure ())) m
    loop bus repl name lastId = do
      ps <- atomically (awaitSince bus [name] lastId)
      if null ps
        then threadDelay 500_000 >> loop bus repl name lastId
        else do
          done <- processPosts cfg bus repl ps
          let newId = maximum (map stamp ps) + 1
          unless done $ loop bus repl name newId

processPosts ::
  ConnectorConfig ->
  Bus Text ->
  ProcEnds Text Text Text ->
  [Stamped Text] ->
  IO Bool
processPosts cfg bus repl = go
  where
    name = connName cfg
    go [] = pure False
    go (Stamped _pid _ts post : rest) = do
      if deliversTo post [name] || name `elem` to post
        then do
          let cmd = body post
          if cmd == "quit" || cmd == ":quit"
            then scribeIO bus (mkPost name [] "closing") >> pure True
            else do
              hPutStrLn stderr $ "connector: " <> T.unpack (T.take 100 cmd)
              commitText (procStdio repl) cmd
              mOut <- timeout (connCommandTimeout cfg) (emitText (procStdio repl))
              let outText = fromMaybe "<command deadline expired>" mOut
              errText <- drainStderr (procStderr repl)
              let reply =
                    T.unlines
                      [ "-- stdout --",
                        outText,
                        "",
                        "-- stderr --",
                        errText
                      ]
              void $ scribeIO bus (mkPost name [from post] reply)
              go rest
        else go rest

-- | Commit one 'Text' token through an 'Ends' conjoint.
commitText :: Ends (Kleisli IO) Text b -> Text -> IO ()
commitText e t = runKleisli (commit (conjoint e) outU) t
  where
    Ends _ outU = open

-- | Emit one 'Text' frame from an 'Ends' companion.  Blocks until a
-- complete frame arrives.
emitText :: Ends (Kleisli IO) a Text -> IO Text
emitText e = runKleisli (emit (companion e) inU) ()
  where
    Ends inU _ = open

-- | Drain pending stderr lines, bounded: returns after ~100ms of quiet.
-- Stderr is diagnostics, not dialogue — a bounded drain, not a blocking
-- read, so a silent stderr cannot stall the turn.
drainStderr :: Ends (Kleisli IO) a Text -> IO Text
drainStderr e = go []
  where
    go acc = do
      m <- timeout 100_000 (emitText e)
      case m of
        Nothing -> pure (T.unlines (reverse acc))
        Just l -> go (l : acc)
