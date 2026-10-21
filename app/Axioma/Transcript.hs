{-# LANGUAGE OverloadedStrings #-}

-- | transcript oracle (B1): a JSONL record per cliQuery, scrub agreement,
-- silent failure when the directory is missing.
module Axioma.Transcript
  ( transcriptTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (Post (..))
import Circuit.Agent.Tensor (AgentShard, closeShardIO)
import Data.IORef (newIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliQuery, parseSessionId)
import Free.Agent.Host (cliHost, hostShard)
import System.Directory (doesFileExist, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

transcriptTopic :: IO ()
transcriptTopic = do
  -------------------------------------------------------------------------
  -- Transcript oracle (B1): cliQuery appends a JSONL record when
  -- cliTranscript is set, and scrubbing the recorded raw reproduces the
  -- posted reply body.  Failure is silent — a missing directory does not
  -- prevent the seat from working.
  -------------------------------------------------------------------------
  putStrLn "Transcript oracle"

  do
    dir <- tempRoot "free-agent-transcript-axioma"
    let script = dir </> "fake-cli.sh"
        sf = dir </> "session"
        transcriptLog = dir </> "transcripts" </> "fake.jsonl"
        badTranscriptLog = script </> "transcripts" </> "fake.jsonl"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-tid'",
            "echo 'hello from stderr' >&2",
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
    -- Part 1: transcript disabled → no file written, cliQuery still works
    result1 <- cliQuery cli "hello"
    exists1 <- doesFileExist transcriptLog
    assert "transcript not written when cliTranscript is Nothing" (not exists1)
    assert "cliQuery still works without transcript" $ "stdin:hello" `T.isInfixOf` result1

    -- Part 2: transcript enabled → one record written per cliQuery call
    postRef <- newIORef 42
    let cliEnabled = cli {cliTranscript = Just (postRef, transcriptLog)}
    result2 <- cliQuery cliEnabled "hi there"
    assert "cliQuery with transcript still produces reply" $ "stdin:hi there" `T.isInfixOf` result2
    -- Check transcript file exists and has one valid JSONL record
    tlog <- TIO.readFile transcriptLog
    let tlines = filter (not . T.null) (T.lines tlog)
    assert "transcript has exactly one record" $ length tlines == 1
    line <- case tlines of
      [x] -> pure x
      _ -> error "expected exactly one transcript record"
    assert "transcript record has post_id:42" $ "\"post_id\":42" `T.isInfixOf` line
    assert "transcript record has exit_code:0" $ "\"exit_code\":0" `T.isInfixOf` line
    assert "transcript record has session_id" $ "\"session_id\":\"fake-tid\"" `T.isInfixOf` line
    assert "transcript record contains raw" $ "\"raw\":\"" `T.isInfixOf` line
    assert "transcript raw has stdout content" $ "stdin:hi there" `T.isInfixOf` line
    assert "transcript raw has stderr content" $ "hello from stderr" `T.isInfixOf` line

    -- Part 3: silent failure — write to an impossible path
    let cliBad = cli {cliTranscript = Just (postRef, badTranscriptLog)}
    result3 <- cliQuery cliBad "still works"
    assert "cliQuery works despite broken transcript path" $ "stdin:still works" `T.isInfixOf` result3

  -- Part 4: full-path test through cliHost + hostShard with transcript
  -- enabled.  Two posts → two cliQuery invocations → two transcript records.
  -- Verify cliScrub(raw) reproduces the reply body.
  --
  -- Mutation guard: if someone drops writeTranscript from cliQuery, the
  -- transcript log stays empty and this test fails at the length check.

  do
    dir <- tempRoot "free-agent-transcript-host-axioma"
    let script = dir </> "fake-echo.sh"
        sf = dir </> "session"
        transcriptLog = dir </> "transcripts" </> "fake.jsonl"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-host'",
            "echo 'diagnostic noise' >&2",
            "printf 'echo:'",
            "cat"
          ]
    let p1 = mkPost "human" ["fake"] "hello"
        p2 = mkPost "human" ["fake"] "world"
    postRef <- newIORef 0
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = cleanCliOut,
              cliStderr = StderrDrop,
              cliStderrTee = Nothing,
              cliTranscript = Just (postRef, transcriptLog)
            }
        h = cliHost "fake" cli
        sh :: AgentShard [Post Text] [Post Text]
        sh = hostShard h
    -- Post 1
    writeIORef postRef 1
    (outs1, st1) <- closeShardIO sh [p1] []
    assert "hostShard single post emits 1 reply" $ length outs1 == 1
    assert "hostShard buffer cleared after single" $ st1 == []
    -- Post 2
    writeIORef postRef 2
    (outs2, st2) <- closeShardIO sh [p2] []
    assert "hostShard second post emits 1 reply" $ length outs2 == 1
    assert "hostShard buffer cleared after second" $ st2 == []
    -- Transcript: 2 records, one per post
    tlog <- TIO.readFile transcriptLog
    let tlines = filter (not . T.null) (T.lines tlog)
    assert "transcript has 2 records (one per post)" $ length tlines == 2
    let line1 = tlines !! 0
        line2 = tlines !! 1
    assert "record 1 has post_id:1" $ "\"post_id\":1" `T.isInfixOf` line1
    assert "record 2 has post_id:2" $ "\"post_id\":2" `T.isInfixOf` line2
    assert "record 1 raw has reply content" $ "echo:hello" `T.isInfixOf` line1
    assert "record 2 raw has reply content" $ "echo:world" `T.isInfixOf` line2
    -- raw contains what cleanCliOut strips (session_id line, stderr noise)
    assert "record 1 raw has session_id line" $ "session_id:" `T.isInfixOf` line1
    assert "record 1 raw has stderr noise" $ "diagnostic noise" `T.isInfixOf` line1
    -- replies are scrubbed: no session_id, no stderr noise
    case (outs1, outs2) of
      ([reply1], [reply2]) -> do
        assert "reply 1 body is echo:hello" $ body reply1 == "echo:hello"
        assert "reply 2 body is echo:world" $ body reply2 == "echo:world"
        assert "reply 1 has no session_id line" $
          not ("session_id:" `T.isInfixOf` body reply1)
        assert "reply 1 has no stderr noise" $
          not ("diagnostic noise" `T.isInfixOf` body reply1)
      _ -> assert "expected one reply per post" False
