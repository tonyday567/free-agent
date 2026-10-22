{-# LANGUAGE OverloadedStrings #-}

-- | host shard round-trips: mkHost echo sketches (BodyWords / BodyLines),
-- a real process host, and a live CLI host (fake binary, exact oracle with
-- session scrape and resume).
module Axioma.Seat
  ( seatTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (AgentShard, Post (..), closeShardIO)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Cli (Cli (..), StderrPolicy (..), parseSessionId)
import Free.Agent.Host (BodyMode (..), Host (..), cliHost, hostShard, mkHost, processHost)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

seatTopic :: IO ()
seatTopic = do
  -------------------------------------------------------------------------
  -- Host shard round-trip
  -------------------------------------------------------------------------
  putStrLn "Host shard round-trip"

  do
    let h = mkHost "echo" (pure . map ("echo:" <>))
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi there"
    (outs, st) <- closeShardIO sh [p] []
    assert "host replies to sender" $
      map body outs == ["echo:hi", "echo:there"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
        && all (\x -> thread x == []) outs
    assert "host buffer cleared after emit" $ st == []

  do
    let h = mkHost "echo" (pure . map ("echo:" <>))
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        posts =
          [ mkPost "human" ["echo"] "one two",
            mkPost "human" ["echo"] "three"
          ]
    (outs, st) <- closeShardIO sh posts []
    assert "host emits one reply batch per committed post" $
      map body outs == ["echo:one", "echo:two", "echo:three"]
        && length outs == 3
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
    assert "multi-post host buffer cleared after emit" $ st == []

  do
    let h = (mkHost "echo" (pure . map ("echo:" <>))) {hostBodyMode = BodyLines}
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["echo"] "hi\nthere"
    (outs, st) <- closeShardIO sh [p] []
    assert "host BodyLines splits on newlines" $
      map body outs == ["echo:hi", "echo:there"]
        && all (\x -> to x == ["human"]) outs
        && all (\x -> from x == "echo") outs
    assert "BodyLines host buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Real-host sketch: external process
  -------------------------------------------------------------------------
  putStrLn "Real-host sketch"

  do
    let h = processHost "shell" "echo" ["hello"]
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["shell"] "ignored"
    (outs, st) <- closeShardIO sh [p] []
    assert "process host runs echo and replies" $
      map body outs == ["hello ignored"] && all (\x -> to x == ["human"]) outs
    assert "process host buffer cleared after emit" $ st == []

  -------------------------------------------------------------------------
  -- Real-host sketch: live CLI host (fake binary, exact oracle)
  -------------------------------------------------------------------------
  putStrLn "CLI host sketch"

  do
    dir <- tempRoot "free-agent-cli-axioma"
    let script = dir </> "fake-cli.sh"
        sf = dir </> "session"
    se <- doesFileExist sf
    when se (removeFile sf)
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo \"session_id: free-fake\"",
            "echo \"argv:$*\"",
            "printf 'stdin:'",
            "cat"
          ]
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliTranscript = Nothing
            }
        h = cliHost "fake" cli
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
        p = mkPost "human" ["fake"] "hello\nworld"
    (outs, st) <- closeShardIO sh [p] []
    assert "cli host preserves multi-line body verbatim" $
      case outs of
        [o] -> "stdin:hello\nworld" `T.isInfixOf` body o && to o == ["human"]
        _ -> False
    assert "cli host buffer cleared after emit" $ st == []
    stored <- TIO.readFile sf
    assert "cli host session id persisted" $ T.strip stored == "free-fake"
    (outs2, _) <- closeShardIO sh [p] []
    assert "cli host second close resumes stored session" $
      case outs2 of
        [o] -> "argv:--resume free-fake" `T.isInfixOf` body o
        _ -> False
