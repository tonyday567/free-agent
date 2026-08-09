{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistent REPL agent as a bus participant.
--
-- 'runConnector' spawns a process (via 'openRepl'), attaches to the
-- free-agent bus, and loops: await addressed posts → commit body to repl
-- stdin → poll stdout until boundary → scribe reply.
--
-- The address model is structural: posts carry 'to :: [Name]' fields and
-- 'deliversTo' performs routing.  No text-prefix parsing needed.
module Free.Agent.Connector
  ( ConnectorConfig (..),
    defaultConnectorConfig,
    runConnector,
  )
where

import Circuit.Agent (Name, Post (..), deliversTo, mkPost)
import Circuit.Agent.Framing (Stamped (..), StoredPost)
import Circuit.Agent.Repl
  ( Repl (..),
    ReplConfig (..),
    defaultReplConfig,
    openRepl,
  )
import Circuit.Ends (Ends (..), HasUnit (..), commit, emit, open)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, unless, void)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Bus (Bus, awaitSince, closeBus, openBus, scribeIO)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Prelude

-- | Configuration for a repl connector.
data ConnectorConfig = ConnectorConfig
  { connName :: Name,
    connBusRoot :: FilePath,
    connRepl :: ReplConfig,
    -- | Does this line mark a repl prompt boundary?
    connBoundary :: Text -> Bool,
    -- | Microseconds to wait for the initial prompt.
    connStartupTimeout :: Int,
    -- | Microseconds per command wait before giving up.
    connCommandTimeout :: Int
  }

-- | Defaults: @cabal repl@ on the circuits project, with a GHCi prompt
-- boundary.  Override 'connRepl', 'connBoundary', and 'connBusRoot'.
defaultConnectorConfig :: Name -> ConnectorConfig
defaultConnectorConfig name =
  ConnectorConfig
    { connName = name,
      connBusRoot = "/tmp/free-agent-bus",
      connRepl =
        defaultReplConfig
          { replCommand = "cabal",
            replArgs = ["repl"],
            replStdinPath = "/tmp/repl-stdin",
            replStdoutPath = "/tmp/repl-stdout.md",
            replStderrPath = "/tmp/repl-stderr.md"
          },
      connBoundary = isGhciPrompt,
      connStartupTimeout = 180_000_000,
      connCommandTimeout = 60_000_000
    }

-- | Standard GHCi prompt boundaries.
isGhciPrompt :: Text -> Bool
isGhciPrompt t =
  "ghci> " `T.isSuffixOf` t
    || "λ> " `T.isSuffixOf` t
    || "> " `T.isSuffixOf` t

-- | Run the connector.  Blocks until the bus is shut down externally,
-- or a @quit@ command arrives (body is exactly @"quit"@ or @":quit"@).
runConnector :: ConnectorConfig -> IO ()
runConnector cfg = do
  -- Wipe and recreate the session directory so logs and FIFOs start fresh.
  let dir = takeDirectory (replStdinPath (connRepl cfg))
  removePathForcibly dir
  createDirectoryIfMissing True dir
  let name = connName cfg
  bus <- openBus (connBusRoot cfg)
  repl <- openRepl (connRepl cfg)
  void $ scribeIO bus (mkPost name [] ("starting repl: " <> T.pack (replCommand (connRepl cfg)) <> " " <> T.unwords (map T.pack (replArgs (connRepl cfg)))))

  -- Drain initial prompt.
  _ <- emitUntil (connBoundary cfg) (connStartupTimeout cfg) (replStdOut repl) >>= \case
    Nothing ->
      hPutStrLn stderr "connector: timed out waiting for initial prompt"
    Just _ -> pure ()
  err0 <- emitPoll (replStdErr repl)
  unless (null err0) $
    forM_ err0 $ \l ->
      hPutStrLn stderr ("connector stderr: " <> T.unpack l)

  -- Main loop: poll for posts, feed each body to the repl, post replies.
  void $ scribeIO bus (mkPost name [] "repl ready")
  loop bus repl name 0
  closeBus bus
  replClose repl
  where
    loop bus repl name lastId = do
      ps <- atomically (awaitSince bus [name] lastId)
      if null ps
        then do
          threadDelay 500_000
          loop bus repl name lastId
        else do
          done <- processPosts cfg bus repl ps
          let newId = maximum (map stampId ps) + 1
          if done
            then pure ()
            else loop bus repl name newId

processPosts ::
  ConnectorConfig -> Bus -> Repl [Text] [Text] [Text] ->
  [StoredPost] -> IO Bool
processPosts cfg bus repl = go
  where
    name = connName cfg
    go [] = pure False
    go (Stamped _pid _ts post : rest) = do
      if deliversTo post [name] || name `elem` to post
        then do
          let cmd = body post
          if cmd == "quit" || cmd == ":quit"
            then do
              void $ scribeIO bus (mkPost name [] "closing")
              pure True
            else do
              hPutStrLn stderr $ "connector: " <> T.unpack (T.take 100 cmd)
              commitLines (replStdOut repl) [cmd]
              outLines <-
                emitUntil (connBoundary cfg) (connCommandTimeout cfg) (replStdOut repl) >>= \case
                  Nothing -> do
                    hPutStrLn stderr "connector: no new prompt; continuing"
                    pure []
                  Just ls -> pure ls
              errLines <- emitPoll (replStdErr repl)
              let reply =
                    T.unlines $
                      ["-- stdout --"]
                        <> outLines
                        <> [""]
                        <> ["-- stderr --"]
                        <> errLines
              void $ scribeIO bus (mkPost name [from post] reply)
              go rest
        else go rest

-- | Commit lines through a repl stdout conjoint (shared stdin).
commitLines :: Ends (Kleisli IO) [Text] [Text] -> [Text] -> IO ()
commitLines e ts = runKleisli (commit (conjoint e) outU) ts
  where
    Ends _ outU = open

-- | One poll emit through a repl companion.
emitPoll :: Ends (Kleisli IO) [Text] [Text] -> IO [Text]
emitPoll e = runKleisli (emit (companion e) inU) ()
  where
    Ends inU _ = open

-- | Poll emit until boundary predicate or timeout (microseconds).
emitUntil ::
  (Text -> Bool) -> Int -> Ends (Kleisli IO) [Text] [Text] -> IO (Maybe [Text])
emitUntil p t e = go 0 [] 10000
  where
    go elapsed acc delay = do
      news <- emitPoll e
      let acc' = acc <> news
      if any p news
        then pure (Just (filter (not . p) acc'))
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= t
            then pure Nothing
            else do
              threadDelay delay
              let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' acc' delay'
