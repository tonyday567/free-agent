{-# LANGUAGE OverloadedStrings #-}

-- | Transcript oracle: per-delivery causation, raw/scrub agreement, and
-- non-fatal transcript sink failure.
module Axioma.Transcript
  ( transcriptTopic,
  )
where

import Axioma.Common (assert, tempRoot)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliDeliverTo, parseSessionId)
import Free.Agent.Delivery (TurnResult (..), awaitTurn)
import System.Directory (removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

transcriptTopic :: IO ()
transcriptTopic = do
  putStrLn "Transcript oracle"
  do
    dir <- tempRoot "free-agent-transcript-axioma"
    let script = dir </> "fake-cli.sh"
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
              cliSessionFile = dir </> "session",
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing
            }
    result <- cliDeliverTo cli transcriptLog 42 "hi there" >>= awaitTurn
    assert "delivery returns the response and writes its transcript" $
      case result of
        TurnResponse response -> "stdin:hi there" `T.isInfixOf` response
        TurnCancelled -> False
    tlog <- TIO.readFile transcriptLog
    let records = filter (not . T.null) (T.lines tlog)
    assert "transcript has exactly one record" $ length records == 1
    case records of
      [line] -> do
        assert "transcript record has explicit post_id:42" $ "\"post_id\":42" `T.isInfixOf` line
        assert "transcript record has exit_code:0" $ "\"exit_code\":0" `T.isInfixOf` line
        assert "transcript record has settled outcome" $ "\"outcome\":\"settled\"" `T.isInfixOf` line
        assert "transcript record has session_id" $ "\"session_id\":\"fake-tid\"" `T.isInfixOf` line
        assert "transcript raw has stdout and stderr" $
          "stdin:hi there" `T.isInfixOf` line && "hello from stderr" `T.isInfixOf` line
      _ -> assert "expected one transcript record" False

    badResult <- cliDeliverTo cli badTranscriptLog 43 "still works" >>= awaitTurn
    assert "delivery works despite a broken transcript path" $
      case badResult of
        TurnResponse response -> "stdin:still works" `T.isInfixOf` response
        TurnCancelled -> False
    removePathForcibly dir

  do
    dir <- tempRoot "free-agent-transcript-host-axioma"
    let script = dir </> "fake-echo.sh"
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
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = dir </> "session",
              cliSessionId = parseSessionId,
              cliStale = \code _ -> code /= ExitSuccess,
              cliScrub = cleanCliOut,
              cliStderr = StderrDrop,
              cliStderrTee = Nothing
            }
    result1 <- cliDeliverTo cli transcriptLog 1 "hello" >>= awaitTurn
    result2 <- cliDeliverTo cli transcriptLog 2 "world" >>= awaitTurn
    assert "two deliveries return independently scrubbed responses" $
      result1 == TurnResponse "echo:hello" && result2 == TurnResponse "echo:world"
    tlog <- TIO.readFile transcriptLog
    let records = filter (not . T.null) (T.lines tlog)
    assert "two deliveries write two transcript records" $ length records == 2
    case records of
      [line1, line2] -> do
        assert "record post ids follow their deliveries" $
          "\"post_id\":1" `T.isInfixOf` line1 && "\"post_id\":2" `T.isInfixOf` line2
        assert "raw evidence retains content stripped from responses" $
          "session_id:" `T.isInfixOf` line1 && "diagnostic noise" `T.isInfixOf` line1
      _ -> assert "expected two transcript records" False
    removePathForcibly dir
