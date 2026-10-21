{-# LANGUAGE OverloadedStrings #-}

-- | ACP seat (R2a): scripted double + wired-binary integration.
module Axioma.Acp
  ( acpTopic,
  )
where

import Axioma.Common (assert, mkPost)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (stamped, unframeStored)
import Circuit.Agent.Mark (Mark (..), markGlyph)
import Circuit.Agent.StdPorts (DoubleStep (..), scriptedDouble)
import Circuit.Agent.Tensor (closeShardIO)
import Circuit.Parser.Json (decodeJson)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Free.Agent.Acp (AcpClient (..), acpHost, acpInitialize, acpNewSession, defaultAcpConfig, jsonToInt)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File (writeCursor)
import Free.Agent.Json (objLookup)
import Free.Agent.Seat (hostSeat, interpretSeat)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)

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
          Right j -> objLookup "id" j >>= jsonToInt
          Left _ -> Nothing
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
        seat = hostSeat (acpHost "acp-seat" 1_000_000 "You are terse." client sid)
        ask = mkPost "human" ["acp-seat"] "hello"
    (outs, _) <- closeShardIO (interpretSeat seat) [ask] []
    assert "R2a the ACP seat answers from the streamed chunk" $
      case outs of
        [o] -> from o == "acp-seat" && body o == "acp says hi"
        _ -> False

  -- Integration: the wired `free-agent acp` binary against a fake peer on
  -- real pipes, on a tmpdir bus.  The cursor is pinned to 0 up front (the
  -- cold-boot default would skip the script).
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-r2a-acp"
        script = root </> "fake-acp.sh"
        promptFile = root </> "prompt.md"
        peer =
          unlines
            [ "#!/bin/sh",
              "while IFS= read -r line; do",
              "  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
              "  case \"$line\" in",
              "    *'\"method\":\"initialize\"'*)\n      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":1,\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"0\"}}}\\n' \"$id\" ;;",
              "    *'\"method\":\"session/new\"'*)\n      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"sessionId\":\"fake-bus\"}}\\n' \"$id\" ;;",
              "    *'\"method\":\"session/prompt\"'*)\n      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"fake-bus\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"acp ack: ok\"}}}}'\n      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
              "  esac",
              "done"
            ]
    removePathForcibly root
    createDirectoryIfMissing True root
    TIO.writeFile promptFile "You are a test seat."
    TIO.writeFile script (T.pack peer)
    setFileMode script 0o755
    writeCursor root "acp1" 0
    myPath <- takeDirectory <$> getExecutablePath
    let bin = myPath </> ".." </> ".." </> ".." </> "free-agent" </> "build" </> "free-agent" </> "free-agent"
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
    _ <- postLocal root (mkPost "human" ["acp1"] (markGlyph Landed <> " done"))
    code <- takeMVar done
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        replies = [stamped s | s <- parsed, from (stamped s) == "acp1"]
    assert "R2a the wired acp seat answers the bus through a real pipe" $
      code == ExitSuccess
        && case replies of
          [r] -> "acp ack: ok" `T.isInfixOf` body r && thread r == [0]
          _ -> False
    removePathForcibly root
