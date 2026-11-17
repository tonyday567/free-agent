{-# LANGUAGE OverloadedStrings #-}

-- | dsh ACP configuration and terminal-outcome witnesses using a local fake.
module Axioma.DshAcp
  ( dshAcpTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Post qualified
import Circuit.Shared (stamped)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Acp (AcpConfig (..), defaultAcpConfig)
import Free.Agent.Backend
import Free.Agent.Bus (postLocal, readLogStrict)
import Free.Agent.Bus.File (writeCursor)
import Free.Agent.Mark (Mark (..), markGlyph)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, findExecutable, removePathForcibly)
import System.Environment (getEnv, getExecutablePath, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

requireFile :: FilePath -> IO ()
requireFile path = go (100 :: Int)
  where
    go 0 = error ("timed out waiting for " <> path)
    go n = do
      exists <- doesFileExist path
      unless exists (threadDelay 20_000 >> go (n - 1))

drainEvents :: Session -> IO [BackendEvent]
drainEvents session = do
  events <- timeout 10_000_000 (go [])
  assert "dsh ACP settles within the local witness deadline" (isJust events)
  pure (fromMaybe [] events)
  where
    go acc = do
      event <- nextEvent session
      case event of
        Output _ -> go (event : acc)
        Settled _ -> pure (reverse (event : acc))

runRequest :: Session -> BackendRequest -> IO [BackendEvent]
runRequest session request = do
  accepted <- sendSession session request
  assert "dsh ACP request accepted" (accepted == Right ())
  drainEvents session

isCompletedWith :: Text -> [BackendEvent] -> Bool
isCompletedWith reply [Output (BackendOutput ReplyOutput actual), Settled (Completed _)] = actual == reply
isCompletedWith _ _ = False

fakeDsh :: FilePath -> Text
fakeDsh root =
  T.pack $
    unlines
      [ "#!/bin/sh",
        "printf 'home=%s cwd=%s args=%s\\n' \"$DSH_HOME\" \"$PWD\" \"$*\" >> \"" <> root </> "provenance" <> "\"",
        "while IFS= read -r line; do",
        "  printf '%s\\n' \"$line\" >> \"" <> root </> "requests" <> "\"",
        "  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
        "  case \"$line\" in",
        "    *'\"method\":\"initialize\"'*)",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":1,\"capabilities\":{},\"serverInfo\":{\"name\":\"fake-dsh\",\"version\":\"0\"}}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/new\"'*)",
        "      echo new >> \"" <> root </> "methods" <> "\"",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"sessionId\":\"fake-dsh-session\",\"configOptions\":[]}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/resume\"'*)",
        "      echo resume >> \"" <> root </> "methods" <> "\"",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"configOptions\":[]}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/prompt\"'*'cancel-turn'*)",
        "      touch \"" <> root </> "cancel-started" <> "\"",
        "      IFS= read -r cancelLine",
        "      printf '%s\\n' \"$cancelLine\" >> \"" <> root </> "requests" <> "\"",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"cancelled\"}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/prompt\"'*'exhaust-turn'*)",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"max_tokens\"}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/prompt\"'*'empty-turn'*)",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/prompt\"'*'resume-turn'*)",
        "      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"fake-dsh-session\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"dsh-resumed\"}}}}'",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/prompt\"'*'second-turn'*)",
        "      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"fake-dsh-session\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"dsh-two\"}}}}'",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
        "    *'\"method\":\"session/prompt\"'*)",
        "      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"method\":\"session/update\",\"params\":{\"update\":{\"sessionId\":\"fake-dsh-session\",\"sessionUpdate\":\"agent_message_chunk\",\"content\":{\"type\":\"text\",\"text\":\"dsh-one\"}}}}'",
        "      printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
        "  esac",
        "done"
      ]

writeFake :: FilePath -> IO ()
writeFake root = do
  createDirectoryIfMissing True (root </> "bin")
  createDirectoryIfMissing True (root </> "workspace")
  createDirectoryIfMissing True (root </> "home")
  TIO.writeFile (root </> "bin" </> "dsh") (fakeDsh root)
  setFileMode (root </> "bin" </> "dsh") 0o755
  TIO.writeFile (root </> "base.yml") "base: true\n"
  TIO.writeFile (root </> "model.yml") "model: true\n"

withFakePath :: FilePath -> IO a -> IO a
withFakePath root action =
  bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
    setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
    action

backendAt :: FilePath -> Backend
backendAt root =
  dshAcpBackend
    (Just "dsh system card")
    (root </> "base.yml")
    (Just (root </> "model.yml"))
    (root </> "workspace")
    (root </> "home")
    (root </> ".sessions" </> "seat.sid")

dshAcpTopic :: IO ()
dshAcpTopic = do
  putStrLn "dsh ACP persistent client"

  -- Fresh session, repeated turns, terminal stop reasons, and process restart.
  do
    root <- tempRoot "free-agent-dsh-acp"
    writeFake root
    withFakePath root $ do
      session <- openBackend (backendAt root)
      assert "dsh ACP declares its named kind, server session, collection, and cancellation" $
        sessionCapabilities session == BackendCapabilities DshAcpBackend ServerSessions CollectedEvents True
      first <- runRequest session (defaultRequest "first-turn")
      second <- runRequest session (defaultRequest "second-turn")
      exhausted <- runRequest session (defaultRequest "exhaust-turn")
      empty <- runRequest session (defaultRequest "empty-turn")
      assert "fresh and repeated turns use one persistent session" $
        isCompletedWith "dsh-one" first && isCompletedWith "dsh-two" second
      assert "max_tokens becomes typed exhaustion" $
        case exhausted of
          [Settled (Exhausted _ evidence)] -> evidenceSessionId evidence == Just "fake-dsh-session"
          _ -> False
      assert "empty end_turn is explicit rather than silent success" $
        case empty of
          [Settled (Failed EmptyReply _ _)] -> True
          _ -> False
      closeSession session
      resumed <- openBackend (backendAt root)
      resumedEvents <- runRequest resumed (defaultRequest "resume-turn")
      assert "restart resumes the stored dsh session and returns its reply" $
        isCompletedWith "dsh-resumed" resumedEvents
      closeSession resumed
    provenance <- T.lines <$> TIO.readFile (root </> "provenance")
    requests <- T.lines <$> TIO.readFile (root </> "requests")
    methods <- T.lines <$> TIO.readFile (root </> "methods")
    workspace <- canonicalizePath (root </> "workspace")
    sid <- T.strip <$> TIO.readFile (root </> ".sessions" </> "seat.sid")
    assert "both processes pin DSH_HOME, cwd, and ordered patch argv" $
      length provenance == 2
        && all (("home=" <> T.pack (root </> "home")) `T.isPrefixOf`) provenance
        && all (("cwd=" <> T.pack workspace) `T.isInfixOf`) provenance
        && all (("args=acp --patch " <> T.pack (root </> "base.yml") <> " --patch " <> T.pack (root </> "model.yml")) `T.isInfixOf`) provenance
    assert "restart uses session/resume without another session/new" $
      methods == ["new", "resume"] && sid == "fake-dsh-session"
    let prompts = filter ("session/prompt" `T.isInfixOf`) requests
    assert "the card is fresh-only and is not replayed on resume" $
      case prompts of
        [first, second, _, _, resumed] ->
          "dsh system card" `T.isInfixOf` first
            && not ("dsh system card" `T.isInfixOf` second)
            && not ("dsh system card" `T.isInfixOf` resumed)
        _ -> False
    removePathForcibly root

  -- A failed resume is not a stale-session licence: do not initialize twice,
  -- silently start a new session, or overwrite the stored id.
  do
    root <- tempRoot "free-agent-dsh-acp-resume-failure"
    writeFake root
    createDirectoryIfMissing True (root </> ".sessions")
    TIO.writeFile (root </> ".sessions" </> "seat.sid") "lost-session"
    TIO.writeFile (root </> "bin" </> "dsh") $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "while IFS= read -r line; do",
            "  printf '%s\\n' \"$line\" >> \"" <> root </> "requests" <> "\"",
            "  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
            "  case \"$line\" in",
            "    *'\"method\":\"initialize\"'*) printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":1}}\\n' \"$id\" ;;",
            "    *'\"method\":\"session/resume\"'*) printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"error\":{\"code\":-32602,\"message\":\"session cwd does not match\"}}\\n' \"$id\" ;;",
            "    *'\"method\":\"session/new\"'*) echo unexpected-new >> \"" <> root </> "requests" <> "\" ;;",
            "  esac",
            "done"
          ]
    setFileMode (root </> "bin" </> "dsh") 0o755
    withFakePath root $ do
      session <- openBackend (backendAt root)
      refused <- sendSession session (defaultRequest "must-not-run")
      assert "failed ACP resume stays a closed startup failure, not a hidden fresh session" $
        refused == Left SendClosed
          && case sessionStartupFailure session of
            Just (Failed ProtocolFailure why evidence) ->
              "session cwd does not match" `T.isInfixOf` why
                && evidenceSessionId evidence == Just "lost-session"
            _ -> False
      closeSession session
    requests <- TIO.readFile (root </> "requests")
    stored <- TIO.readFile (root </> ".sessions" </> "seat.sid")
    assert "failed ACP resume initializes once and preserves the old session id" $
      T.count "\"method\":\"initialize\"" requests == 1
        && T.count "\"method\":\"session/resume\"" requests == 1
        && not ("session/new" `T.isInfixOf` requests)
        && T.strip stored == "lost-session"
    removePathForcibly root

  -- Cancellation is a real ACP notification and settles as Cancelled once.
  do
    root <- tempRoot "free-agent-dsh-acp-cancel"
    writeFake root
    withFakePath root $ do
      session <- openBackend (backendAt root)
      accepted <- sendSession session (defaultRequest "cancel-turn")
      requireFile (root </> "cancel-started")
      won <- cancelSession session
      events <- drainEvents session
      assert "dsh ACP cancellation wins and preserves typed evidence" $
        accepted == Right ()
          && won
          && case events of
            [Settled (Cancelled evidence)] -> evidenceStopReason evidence == Just "cancelled"
            _ -> False
      closeSession session
    requests <- TIO.readFile (root </> "requests")
    assert "cancellation sends session/cancel on the wire" $
      "\"method\":\"session/cancel\"" `T.isInfixOf` requests
    removePathForcibly root

  -- A timeout closes the protocol session so no later turn can consume the
  -- timed-out request's late response.
  do
    root <- tempRoot "free-agent-acp-timeout"
    let script = root </> "slow-acp.sh"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "while IFS= read -r line; do",
            "  id=$(printf '%s' \"$line\" | sed -n 's/.*\"id\":\\([0-9][0-9]*\\).*/\\1/p')",
            "  case \"$line\" in",
            "    *'\"method\":\"initialize\"'*) printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"protocolVersion\":1}}\\n' \"$id\" ;;",
            "    *'\"method\":\"session/new\"'*) printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"sessionId\":\"slow\"}}\\n' \"$id\" ;;",
            "    *'\"method\":\"session/prompt\"'*) sleep 2; printf '{\"jsonrpc\":\"2.0\",\"id\":%s,\"result\":{\"stopReason\":\"end_turn\"}}\\n' \"$id\" ;;",
            "  esac",
            "done"
          ]
    setFileMode script 0o755
    session <-
      openBackend $
        acpBackend
          defaultAcpConfig {acpCommand = script, acpArgs = [], acpWorkDir = root}
          500_000
          "slow system"
    assert ("slow ACP starts: " <> show (sessionStartupFailure session)) (sessionStartupFailure session == Nothing)
    events <- runRequest session (defaultRequest "timeout-turn")
    next <- sendSession session (defaultRequest "must-not-run")
    assert "ACP timeout is typed and closes against late responses" $
      next == Left SendClosed
        && case events of
          [Settled (Failed TimeoutFailure _ _)] -> True
          _ -> False
    closeSession session
    removePathForcibly root

  -- Wired CLI: parser flags reach the same constructor and explicit bus act.
  do
    root <- tempRoot "free-agent-dsh-acp-cli"
    writeFake root
    TIO.writeFile (root </> "prompt.md") "CLI dsh card."
    writeCursor root "seat" 0
    _ <- postLocal root (mkPost "human" ["seat"] "first-turn")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
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
    (code, _, err) <-
      withFakePath root $
        readProcessWithExitCode
          bin
          [ "dsh-acp",
            "--root",
            root,
            "--name",
            "seat",
            "--prompt",
            root </> "prompt.md",
            "--dsh-patch",
            root </> "base.yml",
            "--dsh-model-patch",
            root </> "model.yml",
            "--dsh-home",
            root </> "home",
            "--workspace",
            root </> "workspace"
          ]
          ""
    posts <- map stamped <$> readLogStrict (root </> "log.jsonl")
    let replies = [p | p <- posts, Circuit.Post.from p == "seat", not (null (Circuit.Post.to p))]
    assert ("wired dsh-acp command exits cleanly" <> if code == ExitSuccess then "" else ": " <> err) $
      code == ExitSuccess
    assert "wired dsh-acp publishes one sender-directed reply with ancestry" $
      case replies of
        [reply] ->
          Circuit.Post.body reply == "dsh-one"
            && Circuit.Post.to reply == ["human"]
            && Circuit.Post.thread reply == [0]
        _ -> False
    removePathForcibly root
