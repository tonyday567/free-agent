{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistent REPL agent as a bus participant.
--
-- 'runConnector' spawns a process (via 'openProc'), tails the bus log
-- ('tailLog') for posts addressed to it, and loops: addressed post ->
-- commit body to repl stdin -> block on the next stdout frame -> post the
-- reply back via 'postLocal'.
--
-- The seat is file-truth end to end: it reads via 'tailLog' (fsnotify tail
-- on @log.jsonl@) and replies via 'postLocal' (file lock), never through
-- the in-memory 'TVar' bus.  A long-lived seat must observe posts from
-- other processes, which only ever reach the file; waiting on the 'TVar'
-- image deadlocks (the image goes stale the moment an external process
-- posts).
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

import Circuit.Agent (Name, Post (..), PostId, mkPost)
import Circuit.Agent.Framing (Stamped, stamp, stamped)
import Circuit.Agent.StdPorts
  ( Frame (..),
    ProcConfig (..),
    ProcEnds (..),
    defaultProcConfig,
    ghciMarks,
    openProc,
  )
import Circuit.Category (K (..))
import Circuit.Machine (Poles (..))
import Circuit.Syntax (eval, run)
import Control.Monad (unless, void)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File
  ( Flow (..),
    readCursor,
    tailLog,
    writeCursor,
  )
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Prelude hiding (read)

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
      root = connBusRoot cfg
  repl <- runK (eval (openProc encodeUtf8 decodeUtf8 (connRepl cfg))) ()
  void $ postLocal root (mkPost name [] ("starting repl: " <> T.pack (procCommand (connRepl cfg)) <> " " <> T.unwords (map T.pack (procArgs (connRepl cfg)))))

  -- Drain the initial frame (up to the first prompt).  A dead repl is a
  -- terminal frame, not a quiet stream: report and stop before tailing.
  mFirst <- timeout (connStartupTimeout cfg) (emitText (procStdio repl))
  case mFirst of
    Nothing -> hPutStrLn stderr "connector: startup deadline expired before first prompt"
    Just (Exited code) -> do
      hPutStrLn stderr ("connector: repl exited during startup (" ++ show code ++ ")")
      exitWith (ExitFailure 1)
    Just (Payload _) -> pure ()

  err0 <- drainStderr (procStderr repl)
  unless (T.null err0) $
    hPutStrLn stderr ("connector stderr: " <> T.unpack (T.take 200 err0))

  -- Main loop: tail the log for posts addressed to us.
  void $ postLocal root (mkPost name [] ("repl ready" :: Text))
  cursor <- readCursor root name
  tailLog (root </> "log.jsonl") [name] cursor Nothing (handlePost cfg repl root)
  procClose repl

-- | One delivered post: quit is control, anything else is an ask.
handlePost ::
  ConnectorConfig ->
  ProcEnds Text Text Text ->
  FilePath ->
  Stamped Text ->
  IO Flow
handlePost cfg repl root stored = do
  let name = connName cfg
      ask = stamped stored
  -- F2: never receive our own posts.
  if from ask == name
    then advance >> pure Continue
    else
      if body ask == "quit" || body ask == ":quit"
        then do
          _ <- postLocal root (mkPost name [] ("closing" :: Text))
          advance
          pure Halt
        else do
          replies <- runTurn cfg repl stored
          traverse_ (postLocal root) replies
          advance
          -- A dead repl ends the connector: the turn's diagnostic is
          -- content-decided, so the halt reads from the log, not from a
          -- hunch.
          pure $ if any (T.isPrefixOf "<exited>" . body) replies then Halt else Continue
  where
    advance = writeCursor root (connName cfg) (snd (stamp stored) + 1)

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
    Just (Exited code) ->
      pure [stampedPost name asker askId ("<exited> " <> T.pack (show code))]
    Just (Payload out) -> do
      mSecond <- timeout (connExtraFrameTimeout cfg) (emitText stdio)
      case mSecond of
        Nothing -> pure [replyPost name asker askId out]
        Just (Payload out2) ->
          pure
            [ replyPost name asker askId out,
              stampedPost name asker askId ("<extra-frame> " <> out2)
            ]
        Just (Exited code) ->
          pure
            [ replyPost name asker askId out,
              stampedPost name asker askId ("<exited> " <> T.pack (show code))
            ]

replyPost :: Name -> Name -> PostId -> Text -> Post Text
replyPost name asker askId bodyText =
  (mkPost name [asker] bodyText) {thread = [askId]}

stampedPost :: Name -> Name -> PostId -> Text -> Post Text
stampedPost name asker askId bodyText =
  (mkPost name [asker] bodyText) {thread = [askId]}

-- | Commit one 'Text' token through an 'Poles' commit leg.
commitText :: Poles () (K IO) Text b -> Text -> IO ()
commitText e t = runK (commit e) t

-- | Emit one 'Text' frame from an 'Poles' emit leg.  Blocks until a
-- complete frame arrives; the child's death arrives as the terminal
-- 'Exited' frame — blocking ends by content.
emitText :: Poles () (K IO) a (Frame Text) -> IO (Frame Text)
emitText e = runK (emit e) ()

-- | Drain pending stderr lines, bounded: returns after ~100ms of quiet.
-- Stderr is diagnostics, not dialogue — a bounded drain, not a blocking
-- read, so a silent stderr cannot stall the turn.
drainStderr :: Poles () (K IO) a (Frame Text) -> IO Text
drainStderr e = go []
  where
    go acc = do
      m <- timeout 100_000 (emitText e)
      case m of
        Nothing -> pure (T.unlines (reverse acc))
        Just (Payload l) -> go (l : acc)
        Just (Exited _) -> pure (T.unlines (reverse acc))
