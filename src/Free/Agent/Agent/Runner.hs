{-# LANGUAGE OverloadedStrings #-}

-- | Generic agent seat loop for the free-agent bus.
--
-- Watches ROOT/log.jsonl for posts addressed to NAME(s), calls the supplied
-- handler for each post, and writes the returned replies back through the
-- scribe with the cursor advanced. This is the common runtime shared by the
-- Hermes, Kimi, direct-API, and external-command seats.
module Free.Agent.Agent.Runner
  ( runAgentLoop,
  )
where

import Circuit.Agent (Name, Post (..), PostId, mkPost)
import Circuit.Agent.Framing (StoredPost, stampId)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus.File
  ( QuiesceConfig (..),
    cursorPath,
    findScribe,
    readCursor,
    scribePost,
    tailLog,
    writeCursor,
  )
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

-- | Start a seat loop.
--
-- The caller supplies the subscription names, the bus root, an optional
-- quiescence config, and a handler that turns one incoming 'StoredPost' into
-- zero or more reply posts. The handler is responsible for decoration,
-- scrubbing, routing, and filtering; this function handles the bus plumbing.
runAgentLoop ::
  -- | Agent name used for cursor and logging.
  Text ->
  -- | Subscription names.
  [Name] ->
  -- | Bus root.
  FilePath ->
  -- | Optional quiescence config.
  Maybe QuiesceConfig ->
  -- | Handler: incoming post -> reply posts.
  (StoredPost -> IO [Post Text]) ->
  IO ()
runAgentLoop agentName names root mQuiesce handlePost = do
  hSetBuffering stdout LineBuffering
  scribe <- findScribe
  TIO.putStrLn $ "🟢 free-agent seat starting: " <> T.intercalate "," names
  TIO.putStrLn $ "   root: " <> T.pack root
  TIO.putStrLn $ "   scribe: " <> T.pack scribe
  let path = root </> "log.jsonl"
      onQuiesce = do
        let qc = maybe (error "quiesce action without config") id mQuiesce
            p = mkPost agentName [qcPitboss qc] ("🟡 quiescent after " <> T.pack (show (qcCycles qc)) <> " empty cycles")
        scribePost scribe root p
  cursor <- readCursor root agentName
  TIO.putStrLn $ "   cursor: " <> T.pack (cursorPath root agentName) <> " @ " <> T.pack (show cursor)
  tailLog path names cursor (fmap (\qc -> (qc, onQuiesce)) mQuiesce) $ \stored -> do
    replies <- handlePost stored
    traverse_ (scribePost scribe root) replies
    writeCursor root agentName (stampId stored + 1)
