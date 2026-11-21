{-# LANGUAGE OverloadedStrings #-}

-- | ACP seat (R2a): scripted double + wired-binary integration.
module Axioma.Acp
  ( acpTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Category (K (..))
import Circuit.Machine (Body (Body))
import Circuit.Parser.Framing (decodeRow)
import Circuit.Parser.Json (Json (..), decodeJson)
import Circuit.Post (Post (..))
import Circuit.Shared (stamped)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Free.Agent.Acp (AcpChannel (..), AcpClient (..), AcpConfig (..), AcpEmission (..), AcpInput (..), AcpPhase (..), PromptResult (..), Update (..), acpBody, acpHost, acpInitialize, acpNewSession, closeAcp, defaultAcpConfig, jsonToInt)
import Free.Agent.Activity
import Free.Agent.Agent (runAgent)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File (writeCursor)
import Free.Agent.Host (hostBody)
import Free.Agent.Mark (Mark (..), markGlyph)
import Free.Agent.StdPorts (DoubleStep (..), scriptedDouble)
import System.Directory (doesFileExist, findExecutable, removePathForcibly)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

acpTopic :: IO ()
acpTopic = do
  -------------------------------------------------------------------------
  -- ACP seat (R2a)
  --
  -- A scripted NDJSON peer answers initialize / session/new / prompt with
  -- a streamed agent_message_chunk — no live kimi.  First on the S-a
  -- double (protocol precision), then through the wired binary on real
  -- pipes (integration).  Counterexample: a peer whose response id does
  -- not echo the request hangs the turn; a chunk that is not a
  -- session/update notification leaves the reply empty.
  -------------------------------------------------------------------------
  putStrLn "ACP seat (R2a)"

  -- Protocol precision: one script step per wire turn.
  do
    let ridOf line = case decodeJson (encodeUtf8 line) of
          Right (JObject ps) -> lookup "id" ps >>= jsonToInt
          _ -> Nothing
        resp rid body = "{\"jsonrpc\":\"2.0\",\"id\":" <> T.pack (show rid) <> ",\"result\":" <> body <> "}"
        initOut line = maybe "" (\r -> resp r "{\"protocolVersion\":1,\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"0\"}}") (ridOf line)
        newOut line = maybe "" (\r -> resp r "{\"sessionId\":\"fake-session\"}") (ridOf line)
        chunkNote = "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"fake-session\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"acp says hi\"}}}}"
        promptOut line = maybe "" (\r -> resp r "{\"stopReason\":\"end_turn\"}") (ridOf line)
    pp <- scriptedDouble [Reply initOut, Reply newOut, Reply2 (const chunkNote) promptOut]
    nref <- newIORef 1
    let client = AcpClient {acpPorts = pp, acpConfig = defaultAcpConfig, acpNextId = nref}
    _ <- acpInitialize client 1_000_000
    (msid, _) <- acpNewSession client 1_000_000 "."
    assert "R2a initialize and session/new yield the peer's session id" $
      msid == Just "fake-session"
    let sid = fromMaybe "fake-session" msid
        sh = hostBody (acpHost "acp-seat" 1_000_000 "You are terse." client sid)
        ask = mkPost "human" ["acp-seat"] "hello"
    outs <- runAgent sh [ask]
    assert "R2a the ACP seat answers from the streamed chunk" $
      case outs of
        [o] -> from o == "acp-seat" && body o == "acp says hi"
        _ -> False

  acpBodyWitness

  -- Integration: the wired `free-agent acp` binary against a fake peer on
  -- real pipes, on a tmpdir bus.  The cursor is pinned to 0 up front (the
  -- cold-boot default would skip the script).
  do
    root <- tempRoot "free-agent-r2a-acp"
    let script = root </> "fake-acp.sh"
        promptFile = root </> "prompt.md"
        peer =
          unlines
            [ "#!/bin/sh",
              "while IFS= read -r line; do",
              "  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
              "  case \"$line\" in",
              "    *'\"method\":\"initialize\"'*)\n      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":1,\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"0\"}}}\\n' \"$id\" ;;",
              "    *'\"method\":\"session/new\"'*)\n      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"sessionId\":\"fake-bus\"}}\\n' \"$id\" ;;",
              "    *'\"method\":\"session/prompt\"'*)\n      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"fake-bus\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"@acp1: acp ack: ok\"}}}}'\n      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
              "  esac",
              "done"
            ]
    TIO.writeFile promptFile "You are a test seat."
    TIO.writeFile script (T.pack peer)
    setFileMode script 0o755
    writeCursor root "acp1" 0
    axiomaPath <- getExecutablePath
    let built =
          takeDirectory axiomaPath
            </> ".."
            </> ".."
            </> ".."
            </> "free-agent"
            </> "build"
            </> "free-agent"
            </> "free-agent"
    builtExists <- doesFileExist built
    bin <-
      if builtExists
        then pure built
        else fromMaybe (takeDirectory axiomaPath </> "free-agent") <$> findExecutable "free-agent"
    done <- newEmptyMVar
    _ <-
      forkIO $ do
        (code, _out, err) <-
          readProcessWithExitCode
            bin
            ["acp", "--root", root, "--name", "acp1", "--prompt", promptFile, "--acp-cmd", script]
            ""
        when (code /= ExitSuccess) (putStrLn err)
        putMVar done code
    _ <- postLocal root (mkPost "human" ["acp1"] "ping")
    _ <- postLocal root (mkPost "human" ["acp1"] (markGlyph Landed))
    code <- takeMVar done
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (either (const Nothing) Just . decodeRow) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "acp1"]
    assert "R2a raw acp reply output stays local without inferred audience or ancestry" $
      code == ExitSuccess && null replies && length parsed == 2
    removePathForcibly root

acpBodyWitness :: IO ()
acpBodyWitness = do
  root <- tempRoot "free-agent-acp-body"
  let evidenceFile = root </> "evidence.txt"
      transcriptFile = root </> "acp-transcript.jsonl"
      activityFile = root </> "activity.jsonl"
  TIO.writeFile evidenceFile "local evidence"
  let ridOf line = case decodeJson (encodeUtf8 line) of
        Right (JObject ps) -> lookup "id" ps >>= jsonToInt
        _ -> Nothing
      resp rid result = "{\"jsonrpc\":\"2.0\",\"id\":" <> T.pack (show rid) <> ",\"result\":" <> result <> "}"
      initOut line = maybe "" (\r -> resp r "{\"protocolVersion\":1}") (ridOf line)
      newOut line = maybe "" (\r -> resp r "{\"sessionId\":\"body-session\"}") (ridOf line)
      thoughtNote = "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"agent_thought_chunk\",\"content\":{\"type\":\"text\",\"text\":\"two thoughts\"}}}}"
      reverseNote = "{\"jsonrpc\":\"2.0\",\"id\":41,\"method\":\"fs/read_text_file\",\"params\":{\"path\":\"" <> T.pack evidenceFile <> "\"}}"
      replyNote text = "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"" <> text <> "\"}}}}"
      promptOut line = maybe "" (\r -> resp r "{\"stopReason\":\"end_turn\"}") (ridOf line)
      usageNote = "{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionUpdate\":\"usage_update\",\"used\":11,\"size\":20}}}"
  pp <-
    scriptedDouble
      [ Reply initOut,
        Reply newOut,
        ReplyMany (\line -> [thoughtNote, reverseNote, replyNote "two replies", promptOut line, usageNote]),
        Quiet,
        ReplyMany (\line -> [replyNote "later reply", promptOut line])
      ]
  nref <- newIORef 1
  let client = AcpClient pp (defaultAcpConfig {acpTranscript = Just transcriptFile}) nref
      acquire = do
        _ <- acpInitialize client 1_000_000
        (msid, _) <- acpNewSession client 1_000_000 "."
        pure (client, fromMaybe "body-session" msid)
      Body initialise commitBody emitBody = acpBody acquire 2_000_000
      step observer channel = do
        (next, emission) <- runK emitBody channel
        _ <- observeAcpEmission observer emission
        pure (next, emission)
  first <- runK initialise (AcpPromptInput "first" (pure ()))
  assert "Body init opens one session and commits its first prompt" $
    acpChannelSession first == "body-session"
      && case acpChannelPhase first of
        AcpAwaiting _ _ -> True
        _ -> False
  observer <- newActivityObserver 0.5
  samples <- newIORef []
  (done, result) <-
    withActivitySampler observer 20_000 (\sample -> modifyIORef' samples (sample :) >> appendActivitySample activityFile sample) $ do
      (thoughtChannel, thought) <- step observer first
      thoughtCount <- currentTokens observer
      assert "the first emit is a live thought chunk before reply or settlement" $
        thoughtCount == TokenCount 2
          && case thought of
            AcpProtocol _ (Just (AgentThoughtChunk "two thoughts")) -> True
            _ -> False
      waitForSamples samples 3
      live <- reverse <$> readIORef samples
      let quietDecay =
            any
              (\(before, after) -> sampleTokens before == sampleTokens after && sampleInstantRate after == 0 && sampleRate after < sampleRate before)
              (zip live (drop 1 live))
      assert ("independent live samples are monotonic, positive, and decay on quiet zeroes: " <> show live) $
        length live >= 3
          && and (zipWith (<) (map sampleTime live) (drop 1 (map sampleTime live)))
          && any ((> 0) . sampleInstantRate) live
          && quietDecay
          && sampleTokens (last live) == TokenCount 2
          && and (zipWith (<=) (map sampleTokens live) (drop 1 (map sampleTokens live)))
      (maintenanceChannel, maintenance) <- step observer thoughtChannel
      maintenanceCount <- currentTokens observer
      assert "reverse RPC is protocol maintenance with no update or token output" $
        maintenanceCount == TokenCount 2
          && case maintenance of
            AcpProtocol _ Nothing -> True
            _ -> False
      (replyChannel, reply) <- step observer maintenanceChannel
      replyCount <- currentTokens observer
      assert "reply text commits independently after thought" $
        replyCount == TokenCount 4
          && case reply of
            AcpProtocol _ (Just (AgentMessageChunk "two replies")) -> True
            _ -> False
      (responseChannel, response) <- step observer replyChannel
      assert "the correlated stop frame starts draining rather than settling early" $
        case (response, acpChannelPhase responseChannel) of
          (AcpProtocol _ Nothing, AcpDraining _ _) -> True
          _ -> False
      (usageChannel, usage) <- step observer responseChannel
      usageCount <- currentTokens observer
      assert "trailing usage stays ordered and does not invent generated tokens" $
        usageCount == TokenCount 4
          && case usage of
            AcpProtocol _ (Just (UsageUpdate _)) -> True
            _ -> False
      (settled, terminal) <- step observer usageChannel
      case terminal of
        AcpPromptDone _ value -> pure (settled, value)
        _ -> error "ACP Body did not settle after its bounded drain"
  assert "one terminal result retains ordered thought, reply, usage, stop, and raw frames" $
    prReply result == "two replies"
      && prThoughts result == "two thoughts"
      && prStopReason result == Just "end_turn"
      && length (prMessages result) == 5
      && case prUpdates result of
        [AgentThoughtChunk _, AgentMessageChunk _, UsageUpdate _] -> True
        _ -> False
  assert "the settled channel is reusable by a later commit" $
    case acpChannelPhase done of
      AcpDone -> True
      _ -> False
  later <- runK commitBody (done, AcpPromptInput "second" (pure ()))
  (laterReplyChannel, laterReply) <- runK emitBody later
  assert "later commit uses the same session and emits its own reply" $
    acpChannelSession laterReplyChannel == "body-session"
      && case laterReply of
        AcpProtocol _ (Just (AgentMessageChunk "later reply")) -> True
        _ -> False
  (laterResponseChannel, _) <- runK emitBody laterReplyChannel
  (laterDone, laterTerminal) <- runK emitBody laterResponseChannel
  assert "later prompt also settles exactly once" $
    case (laterTerminal, acpChannelPhase laterDone) of
      (AcpPromptDone _ value, AcpDone) -> prReply value == "later reply" && prStopReason value == Just "end_turn"
      _ -> False
  closeAcp client
  transcript <- TIO.readFile transcriptFile
  activity <- TIO.readFile activityFile
  busExists <- doesFileExist (root </> "log.jsonl")
  assert "reverse RPC was answered and activity stayed on a local surface" $
    "fs/read_text_file" `T.isInfixOf` transcript
      && "local evidence" `T.isInfixOf` transcript
      && not (T.null activity)
      && not busExists
  removePathForcibly root

waitForSamples :: IORef [ActivitySample] -> Int -> IO ()
waitForSamples samples wanted = do
  found <- timeout 2_000_000 go
  assert "independent activity sampler produces live samples" (found == Just ())
  where
    go = do
      current <- readIORef samples
      if length current >= wanted then pure () else threadDelay 10_000 >> go
