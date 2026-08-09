{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistent REPL agent as a bus participant.
--
-- 'runConnector' spawns a process (via 'openRepl'), attaches to the
-- free-agent bus, and loops: await addressed posts → commit body to repl
-- stdin → poll stdout until boundary → scribe reply.
module Free.Agent.Connector
  ( ConnectorConfig (..),
    defaultConnectorConfig,
    runConnector,
  )
where

import Circuit.Agent (Name, Post (..), deliversTo, mkPost)
import Circuit.Agent.Framing (Stamped (..))
import Circuit.Agent.Repl
  ( Repl (..),
    ReplConfig (..),
    defaultReplConfig,
    openRepl,
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
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import Prelude

data ConnectorConfig = ConnectorConfig
  { connName :: Name,
    connBusRoot :: FilePath,
    connRepl :: ReplConfig,
    connBoundary :: Text -> Bool,
    connStartupTimeout :: Int,
    connCommandTimeout :: Int
  }

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

isGhciPrompt :: Text -> Bool
isGhciPrompt t =
  "ghci> " `T.isSuffixOf` t
    || "\955> " `T.isSuffixOf` t
    || "> " `T.isSuffixOf` t

runConnector :: ConnectorConfig -> IO ()
runConnector cfg = do
  let dir = takeDirectory (replStdinPath (connRepl cfg))
  removePathForcibly dir
  createDirectoryIfMissing True dir
  let name = connName cfg
  bus <- openBus (connBusRoot cfg)
  repl <- runKleisli (run (openRepl encodeUtf8 decodeUtf8 (connRepl cfg))) ()
  void $ scribeIO bus (mkPost name [] ("starting repl: " <> T.pack (replCommand (connRepl cfg)) <> " " <> T.unwords (map T.pack (replArgs (connRepl cfg)))))

  -- Drain initial prompt.
  poll <- emitText (replStdOut repl)
  unless (connBoundary cfg poll) $
    void $ emitUntil (connBoundary cfg) (connStartupTimeout cfg) (replStdOut repl)

  err0 <- emitText (replStdErr repl)
  unless (T.null err0) $
    hPutStrLn stderr ("connector stderr: " <> T.unpack (T.take 200 err0))

  -- Main loop.
  void $ scribeIO bus (mkPost name [] "repl ready")
  loop bus repl name 0
  closeBus bus
  replClose repl
  where
    loop bus repl name lastId = do
      ps <- atomically (awaitSince bus [name] lastId)
      if null ps
        then threadDelay 500_000 >> loop bus repl name lastId
        else do
          done <- processPosts cfg bus repl ps
          let newId = maximum (map stamp ps) + 1
          unless done $ loop bus repl name newId

processPosts ::
  ConnectorConfig -> Bus Text -> Repl Text Text Text ->
  [Stamped Text] -> IO Bool
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
              commitText (replStdOut repl) cmd
              mOut <- emitUntil (connBoundary cfg) (connCommandTimeout cfg) (replStdOut repl)
              let outText = fromMaybe "" mOut
              errText <- emitText (replStdErr repl)
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

-- | Emit one 'Text' token from an 'Ends' companion.
emitText :: Ends (Kleisli IO) a Text -> IO Text
emitText e = runKleisli (emit (companion e) unitIn) ()
  where
    Ends unitIn _ = open

-- | Poll emit until boundary predicate or timeout (microseconds).
emitUntil ::
  (Text -> Bool) -> Int -> Ends (Kleisli IO) Text Text -> IO (Maybe Text)
emitUntil p t e = go 0 "" 10000
  where
    go elapsed acc delay = do
      new <- emitText e
      let acc' = acc <> new
      if p new
        then pure (Just acc')
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= t
            then pure Nothing
            else do
              threadDelay delay
              let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' acc' delay'
