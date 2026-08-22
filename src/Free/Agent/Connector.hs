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
--
-- Turn correlation is content-decided, not geometry-decided.  Each addressed
-- post is treated as an /ask/ whose bus 'stamp' is the correlation id.  The
-- response post carries that id in its 'thread' and is addressed back to the
-- asker.  Zero-frame and two-frame violations are reported as in-band
-- diagnostic posts with the same ask id, so downstream consumers can detect
-- them by the stamped output grammar rather than by timing.
module Free.Agent.Connector
  ( ConnectorConfig (..),
    defaultConnectorConfig,
    runConnector,

    -- * Turn handler (exported for oracles)
    TurnResult (..),
    runTurn,
  )
where

import Circuit.Agent (Name, Post (..), PostId, deliversTo, mkPost)
import Circuit.Agent.Framing (Stamped, stamp, stamped)
import Circuit.Agent.StdPorts
  ( ProcConfig (..),
    ProcEnds (..),
    defaultProcConfig,
    ghciMarks,
    openProc,
  )
import Circuit.Category (K (..))
import Circuit.Poles (Poles (..), HasDual (..), In (..), Out (..))
import Circuit.Layer (run)
import Circuit.Syntax (eval)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (unless, void)
import Data.Foldable (traverse_)
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
    connCommandTimeout :: Int,
    -- | Microseconds to wait for a second frame after the first, used to
    -- detect two-frame commits.  Should be short: the second frame, if it
    -- belongs to the same turn, is already enqueued by the pumper.
    connExtraFrameTimeout :: Int
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
      connCommandTimeout = 60_000_000,
      connExtraFrameTimeout = 10_000
    }

runConnector :: ConnectorConfig -> IO ()
runConnector cfg = do
  let name = connName cfg
  bus <- openBus (connBusRoot cfg)
  repl <- runK (eval (openProc encodeUtf8 decodeUtf8 (connRepl cfg))) ()
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
          let newId = maximum (map (snd . stamp) ps) + 1
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
    go (stored : rest) = do
      if deliversTo (stamped stored) [name] || name `elem` to (stamped stored)
        then do
          let cmd = body (stamped stored)
          if cmd == "quit" || cmd == ":quit"
            then scribeIO bus (mkPost name [] "closing") >> pure True
            else do
              replies <- runTurn cfg repl stored
              traverse_ (scribeIO bus) replies
              go rest
        else go rest

-- | The result of one critical-section turn, before scribing.
data TurnResult
  = -- | Exactly one response frame.
    TurnOk Text
  | -- | No frame arrived before the deadline.
    TurnZeroFrame
  | -- | More than one frame arrived for the same ask.
    TurnMultiFrame [Text]
  deriving (Eq, Show)

-- | Run one critical-section turn for an addressed post.
--
-- The ask is identified by the incoming post's bus 'stamp'.  The response
-- carries that id in its 'thread' and is addressed back to the asker.  This
-- makes correlation content-decided: a dropped, doubled, or misordered frame
-- is observable from the bus log, not inferred from pipe adjacency.
runTurn ::
  ConnectorConfig ->
  ProcEnds Text Text Text ->
  Stamped Text ->
  IO [Post Text]
runTurn cfg repl stored = do
  let ask = stamped stored
      asker = from ask
      askId = snd (stamp stored)
      name = connName cfg
      stdio = procStdio repl

  unless (T.null (body ask)) $ commitText stdio (body ask)

  mFirst <- timeout (connCommandTimeout cfg) (emitText stdio)
  case mFirst of
    Nothing -> pure [stampedPost name asker askId "<zero-frame>"]
    Just out -> do
      mSecond <- timeout (connExtraFrameTimeout cfg) (emitText stdio)
      case mSecond of
        Nothing -> pure [replyPost name asker askId out]
        Just out2 ->
          pure
            [ replyPost name asker askId out,
              stampedPost name asker askId ("<extra-frame> " <> out2)
            ]

replyPost :: Name -> Name -> PostId -> Text -> Post Text
replyPost name asker askId bodyText =
  (mkPost name [asker] bodyText) {thread = [askId]}

stampedPost :: Name -> Name -> PostId -> Text -> Post Text
stampedPost name asker askId bodyText =
  (mkPost name [asker] bodyText) {thread = [askId]}

-- | Commit one 'Text' token through an 'Poles' conjoint.
commitText :: Poles (K IO) Text b -> Text -> IO ()
commitText e t = runK (commit (conjoint e) outU) t
  where
    Poles _ outU = open

-- | Emit one 'Text' frame from an 'Poles' companion.  Blocks until a
-- complete frame arrives.
emitText :: Poles (K IO) a Text -> IO Text
emitText e = runK (emit (companion e) inU) ()
  where
    Poles inU _ = open

-- | Drain pending stderr lines, bounded: returns after ~100ms of quiet.
-- Stderr is diagnostics, not dialogue — a bounded drain, not a blocking
-- read, so a silent stderr cannot stall the turn.
drainStderr :: Poles (K IO) a Text -> IO Text
drainStderr e = go []
  where
    go acc = do
      m <- timeout 100_000 (emitText e)
      case m of
        Nothing -> pure (T.unlines (reverse acc))
        Just l -> go (l : acc)
