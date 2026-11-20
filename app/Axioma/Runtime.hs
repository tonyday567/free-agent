{-# LANGUAGE OverloadedStrings #-}

-- | Live Backend-seat settlement witnesses. These drive the built executable,
-- not the retired runAgentLoop, and use only local fake CLI processes.
module Axioma.Runtime
  ( runtimeTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Post (Post (..), deliversTo)
import Circuit.Shared (stamp, stamped)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (postLocal, readLogStrict)
import Free.Agent.Bus.File (readCursor, readTransaction, writeCursor)
import Free.Agent.Bus.Heartbeat (HbState (Dead), readHeartbeat)
import Free.Agent.Mark (Mark (..), markGlyph)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removeDirectory, removePathForcibly)
import System.Environment (getEnv, getExecutablePath, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (getPid, readProcessWithExitCode, spawnProcess, terminateProcess, waitForProcess)
import System.Timeout (timeout)

seatBinary :: IO FilePath
seatBinary = do
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
  if builtExists
    then pure built
    else fromMaybe (takeDirectory axiomaPath </> "free-agent") <$> findExecutable "free-agent"

logPosts :: FilePath -> IO [Post Text Text]
logPosts root = map stamped <$> readLogStrict (root </> "log.jsonl")

kimiArgs :: FilePath -> Text -> [String]
kimiArgs root name =
  [ "--root",
    root,
    "--name",
    T.unpack name,
    "--backend",
    "kimi",
    "--prompt",
    root </> "prompt.md",
    "--session",
    root </> ".sessions" </> T.unpack name <> ".sid"
  ]

writeSeat :: FilePath -> Text -> Text -> IO ()
writeSeat root name source = do
  let bin = root </> "bin"
  createDirectoryIfMissing True bin
  createDirectoryIfMissing True (root </> ".sessions")
  TIO.writeFile (root </> "prompt.md") "Runtime witness seat."
  TIO.writeFile (root </> "bin" </> "kimi") source
  setFileMode (root </> "bin" </> "kimi") 0o755
  writeCursor root name 0

runKimi :: FilePath -> Text -> IO (ExitCode, String, String)
runKimi root name = do
  bin <- seatBinary
  bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
    setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
    readProcessWithExitCode bin (kimiArgs root name) ""

callLog :: FilePath -> IO Text
callLog root = do
  let path = root </> "calls"
  exists <- doesFileExist path
  if exists then TIO.readFile path else pure ""

waitForPath :: FilePath -> IO ()
waitForPath path = go (100 :: Int)
  where
    go 0 = error ("timed out waiting for " <> path)
    go n = do
      exists <- doesFileExist path
      unless exists (threadDelay 20_000 >> go (n - 1))

scriptPreamble :: FilePath -> String
scriptPreamble root =
  unlines
    [ "#!/bin/sh",
      "printf '%s' \"$*\" | tr '\\n' ' ' >> \"" <> root </> "calls" <> "\"",
      "printf '\\n' >> \"" <> root </> "calls" <> "\""
    ]

successSource :: FilePath -> Text -> Text
successSource root label =
  T.pack (scriptPreamble root)
    <> "printf 'To resume this session: kimi -r runtime-session\\n"
    <> label
    <> "\\n'\n"

runtimeTopic :: IO ()
runtimeTopic = do
  putStrLn "live Backend-seat settlement"

  -- Channel and multi-sender input: one explicit response reaches every
  -- external sender, clears the channel, and cites the whole batch.
  do
    root <- tempRoot "free-agent-runtime-audience"
    writeSeat root "ops" (successSource root "runtime-reply")
    _ <- postLocal root (mkPost "tony" [] "channel question") {channel = Just "ops"}
    _ <- postLocal root (mkPost "alex" ["ops"] "direct question")
    _ <- postLocal root (mkPost "runner" ["ops"] (markGlyph Landed))
    (code, _, err) <- runKimi root "ops"
    posts <- logPosts root
    let replies = [p | p <- posts, from p == "ops", not (null (to p))]
    assert ("channel/multi-sender seat exits cleanly" <> if code == ExitSuccess then "" else ": " <> err) $
      code == ExitSuccess
    assert "reply clears channel, reaches both senders, and cites all inputs" $
      case replies of
        [reply] ->
          channel reply == Nothing
            && to reply == ["tony", "alex"]
            && thread reply == [0, 1]
            && deliversTo reply ["tony"]
            && deliversTo reply ["alex"]
        _ -> False
    removePathForcibly root

  -- A stale stored id gets exactly one explicit fresh attempt and rotates.
  do
    root <- tempRoot "free-agent-runtime-stale"
    let source =
          T.pack (scriptPreamble root)
            <> T.pack
              ( unlines
                  [ "case \"$*\" in",
                    "  *'-r old-sid'*) echo 'Session \"old-sid\" not found'; exit 1 ;;",
                    "esac",
                    "printf 'To resume this session: kimi -r new-sid\\nrecovered\\n'"
                  ]
              )
    writeSeat root "seat" source
    TIO.writeFile (root </> ".sessions" </> "seat.sid") "old-sid"
    _ <- postLocal root (mkPost "human" ["seat"] "stale work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    (code, _, err) <- runKimi root "seat"
    calls <- T.lines <$> callLog root
    sid <- T.strip <$> TIO.readFile (root </> ".sessions" </> "seat.sid")
    cursor <- readCursor root "seat"
    posts <- logPosts root
    assert ("stale recovery exits cleanly" <> if code == ExitSuccess then "" else ": " <> err) $
      code == ExitSuccess
    assert "stale recovery makes one resumed refusal and one fresh success" $
      length calls == 2
        && case calls of
          [resumed, fresh] -> "-r old-sid" `T.isInfixOf` resumed && not ("-r old-sid" `T.isInfixOf` fresh)
          _ -> False
    assert "stale recovery rotates the sid and commits the recovered reply" $
      sid == "new-sid"
        && cursor == 2
        && any (\p -> from p == "seat" && body p == "recovered" && to p == ["human"]) posts
    removePathForcibly root

  -- Interruption leaves the batch unsettled. A later explicit launch is the
  -- declared retry: the cancelled transcript is evidence, not a settlement.
  do
    root <- tempRoot "free-agent-runtime-interrupted"
    let source =
          T.pack (scriptPreamble root)
            <> "echo 'Operation interrupted: waiting for model response (1s elapsed).'; exit 130\n"
    writeSeat root "seat" source
    _ <- postLocal root (mkPost "human" ["seat"] "interrupt work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    (firstCode, _, _) <- runKimi root "seat"
    firstCursor <- readCursor root "seat"
    firstTransaction <- readTransaction root "seat"
    (secondCode, _, _) <- runKimi root "seat"
    calls <- T.lines <$> callLog root
    content <- TIO.readFile (root </> "log.jsonl")
    assert "interrupted attempt stops without committing its input" $
      firstCode == ExitSuccess && firstCursor == 0 && firstTransaction == Nothing
    assert "restart re-presents interrupted input without inventing a settlement" $
      secondCode == ExitSuccess
        && length calls == 2
        && not ("ambiguous recovery" `T.isInfixOf` content)
    removePathForcibly root

  -- Kill the seat after the child starts but before any Backend outcome.
  -- The active-batch marker is the evidence that automatic replay is unsafe.
  do
    root <- tempRoot "free-agent-runtime-active-crash"
    let hanging =
          T.pack (scriptPreamble root)
            <> T.pack ("touch '" <> root </> "turn-started'\nsleep 5\n")
    writeSeat root "seat" hanging
    _ <- postLocal root (mkPost "human" ["seat"] "active crash work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    bin <- seatBinary
    handle <-
      bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
        setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
        spawnProcess bin (kimiArgs root "seat")
    waitForPath (root </> "turn-started")
    pid <- getPid handle
    maybe (terminateProcess handle) (signalProcess sigKILL) pid
    _ <- waitForProcess handle
    active <- doesFileExist (root </> ".active-seat")
    transcript <- doesFileExist (root </> ".transcripts" </> "seat.jsonl")
    firstCalls <- T.lines <$> callLog root
    writeSeat root "seat" (successSource root "must-not-run")
    (secondCode, _, _) <- runKimi root "seat"
    secondCalls <- T.lines <$> callLog root
    cursor <- readCursor root "seat"
    transaction <- readTransaction root "seat"
    content <- TIO.readFile (root </> "log.jsonl")
    assert "a mid-invocation kill leaves active evidence and no invented outcome" $
      active && not transcript && length firstCalls == 1
    assert "restart after a mid-invocation kill is ambiguous and never replays" $
      secondCode == ExitSuccess
        && length secondCalls == 1
        && cursor == 0
        && transaction == Nothing
        && "ambiguous recovery" `T.isInfixOf` content
    removePathForcibly root

  -- Force transaction creation to fail after publication. Restart must retain
  -- the single reply and expose ambiguity without repeating the client turn.
  do
    root <- tempRoot "free-agent-runtime-crash"
    writeSeat root "seat" (successSource root "published-once")
    _ <- postLocal root (mkPost "human" ["seat"] "crash work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    createDirectoryIfMissing False (root </> ".transaction-seat")
    (firstCode, _, _) <- runKimi root "seat"
    firstCalls <- T.lines <$> callLog root
    firstPosts <- logPosts root
    let firstReplies = [p | p <- firstPosts, from p == "seat", body p == "published-once"]
    firstCursor <- readCursor root "seat"
    removeDirectory (root </> ".transaction-seat")
    (secondCode, _, _) <- runKimi root "seat"
    secondCalls <- T.lines <$> callLog root
    secondPosts <- logPosts root
    let secondReplies = [p | p <- secondPosts, from p == "seat", body p == "published-once"]
    assert "forced post-publication crash retains one reply and no cursor" $
      firstCode /= ExitSuccess
        && length firstCalls == 1
        && length firstReplies == 1
        && firstCursor == 0
    assert "ambiguous restart does not duplicate publication or invocation" $
      secondCode == ExitSuccess
        && length secondCalls == 1
        && length secondReplies == 1
        && any (\p -> from p == "seat" && "ambiguous recovery" `T.isInfixOf` body p) secondPosts
    removePathForcibly root

  -- Terminal evidence is not optional: if its append fails, publication and
  -- settlement stop and the surviving active marker prevents a replay.
  do
    root <- tempRoot "free-agent-runtime-transcript-crash"
    writeSeat root "seat" (successSource root "must-not-publish")
    _ <- postLocal root (mkPost "human" ["seat"] "transcript crash work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    createDirectoryIfMissing True (root </> ".transcripts" </> "seat.jsonl")
    (firstCode, _, _) <- runKimi root "seat"
    firstCalls <- T.lines <$> callLog root
    firstPosts <- logPosts root
    heartbeat <- readHeartbeat root "seat"
    removeDirectory (root </> ".transcripts" </> "seat.jsonl")
    (secondCode, _, _) <- runKimi root "seat"
    secondCalls <- T.lines <$> callLog root
    cursor <- readCursor root "seat"
    transaction <- readTransaction root "seat"
    content <- TIO.readFile (root </> "log.jsonl")
    assert "failed transcript evidence blocks publication and leaves a dead heartbeat" $
      firstCode /= ExitSuccess
        && length firstCalls == 1
        && not (any (\p -> from p == "seat" && body p == "must-not-publish") firstPosts)
        && fmap (\(state, _, _) -> state) heartbeat == Just Dead
    assert "missing terminal evidence recovers as ambiguous without another invocation" $
      secondCode == ExitSuccess
        && length secondCalls == 1
        && cursor == 0
        && transaction == Nothing
        && "ambiguous recovery" `T.isInfixOf` content
    removePathForcibly root

  -- Started failure is not silently skipped after a retry ladder. It is
  -- recorded, exits nonzero, and leaves the input for explicit reconciliation.
  do
    root <- tempRoot "free-agent-runtime-failed"
    let source = T.pack (scriptPreamble root) <> "echo 'provider failed'; exit 1\n"
    writeSeat root "seat" source
    _ <- postLocal root (mkPost "human" ["seat"] "failed work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    (code, _, _) <- runKimi root "seat"
    cursor <- readCursor root "seat"
    transaction <- readTransaction root "seat"
    calls <- T.lines <$> callLog root
    content <- TIO.readFile (root </> "log.jsonl")
    transcript <- TIO.readFile (root </> ".transcripts" </> "seat.jsonl")
    assert "started failure exits nonzero without skip, retry, or cursor" $
      code /= ExitSuccess
        && cursor == 0
        && transaction == Nothing
        && length calls == 1
        && "no automatic retry or skip" `T.isInfixOf` content
        && "\"attempt\":1" `T.isInfixOf` transcript
    removePathForcibly root

  -- A failed row is evidence, not settlement. Explicit later launches retry
  -- the same batch and can recover without any cursor or marker rewrite.
  do
    root <- tempRoot "free-agent-runtime-failed-recovery"
    let source =
          T.pack (scriptPreamble root)
            <> T.pack
              ( unlines
                  [ "n=0",
                    "if [ -f '" <> root </> "count' ]; then n=$(cat '" <> root </> "count'); fi",
                    "n=$((n+1)); echo \"$n\" > '" <> root </> "count'",
                    "if [ \"$n\" -lt 3 ]; then echo 'provider failed'; exit 1; fi",
                    "printf 'To resume this session: kimi -r recovered-sid\\nrecovered\\n'"
                  ]
              )
    writeSeat root "seat" source
    _ <- postLocal root (mkPost "human" ["seat"] "failed then recovered")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    (firstCode, _, _) <- runKimi root "seat"
    (secondCode, _, _) <- runKimi root "seat"
    firstCursor <- readCursor root "seat"
    (thirdCode, _, err) <- runKimi root "seat"
    calls <- T.lines <$> callLog root
    cursor <- readCursor root "seat"
    transaction <- readTransaction root "seat"
    posts <- logPosts root
    assert ("explicit launches recover a failed batch without skipping it" <> if thirdCode == ExitSuccess then "" else ": " <> err) $
      firstCode /= ExitSuccess
        && secondCode /= ExitSuccess
        && thirdCode == ExitSuccess
        && firstCursor == 0
        && length calls == 3
        && cursor == 2
        && transaction == Just 1
        && any (\p -> from p == "seat" && to p == ["human"] && body p == "recovered") posts
    removePathForcibly root

  -- Startup and spawn availability are environment failures, not fabricated
  -- failed turns. Neither may advance mail or write a synthetic exit-1 row.
  do
    root <- tempRoot "free-agent-runtime-unavailable"
    writeSeat root "seat" (successSource root "unused")
    bin <- seatBinary
    (startupCode, _, startupErr) <-
      readProcessWithExitCode
        bin
        ["acp", "--root", root, "--name", "seat", "--prompt", root </> "prompt.md", "--acp-cmd", root </> "missing-acp"]
        ""
    startupLog <- doesFileExist (root </> "log.jsonl")
    assert "backend startup failure is a loud non-zero diagnostic before the tail loop" $
      startupCode /= ExitSuccess
        && "backend startup failed" `T.isInfixOf` T.pack startupErr
        && not startupLog
    _ <- postLocal root (mkPost "human" ["seat"] "unavailable work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    (code, _, _) <-
      readProcessWithExitCode
        bin
        ["cmd", "--root", root, "--name", "seat", "--prompt", root </> "prompt.md", "--cmd", root </> "missing-command"]
        ""
    cursor <- readCursor root "seat"
    transaction <- readTransaction root "seat"
    transcriptExists <- doesFileExist (root </> ".transcripts" </> "seat.jsonl")
    assert "unavailable-before-start leaves no failed-turn row and no settlement" $
      code /= ExitSuccess && cursor == 0 && transaction == Nothing && not transcriptExists
    removePathForcibly root

  -- A started failure delivered to an already-active watcher must not leave
  -- the tail offset behind the original post. Its notice is only a wake: the
  -- addressed TASK is the input that must never be presented twice in-process.
  do
    root <- tempRoot "free-agent-runtime-active-failure"
    let failing =
          T.pack (scriptPreamble root)
            <> "printf 'STDOUT-TAIL-MARKER\\n'\nprintf 'STDERR-TAIL-MARKER\\n' >&2\nexit 3\n"
    writeSeat root "seat" failing
    TIO.writeFile (root </> "log.jsonl") ""
    bin <- seatBinary
    handle <-
      bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
        setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
        spawnProcess bin (kimiArgs root "seat" <> ["--quiesce", "3"])
    waitForPath (root </> ".hb-seat")
    threadDelay 200_000
    cause <- postLocal root (mkPost "runner" ["seat"] "TASK active watcher failure")
    exited <- timeout 10_000_000 (waitForProcess handle)
    case exited of
      Nothing -> do
        terminateProcess handle
        _ <- waitForProcess handle
        pure ()
      Just _ -> pure ()
    calls <- T.lines <$> callLog root
    posts <- logPosts root
    cursor <- readCursor root "seat"
    transaction <- readTransaction root "seat"
    transcriptExists <- doesFileExist (root </> ".transcripts" </> "seat.jsonl")
    transcript <- if transcriptExists then TIO.readFile (root </> ".transcripts" </> "seat.jsonl") else pure ""
    active <- doesFileExist (root </> ".active-seat")
    let rows = filter (not . T.null) (T.lines transcript)
        notices =
          [ p
          | p <- posts,
            from p == "seat",
            null (to p),
            "backend failed on posts 0" `T.isInfixOf` body p
          ]
        code = fromMaybe (ExitFailure 124) exited
    putStrLn $
      "active-watcher failure counts: exit="
        <> show code
        <> " calls="
        <> show (length calls)
        <> " rows="
        <> show (length rows)
        <> " notices="
        <> show (length notices)
        <> " cursor="
        <> show cursor
    assert "active-watcher failure is bounded to one invocation, failed row, and notice" $
      isJust exited && length calls == 1 && length rows == 1 && length notices == 1
    assert "active-watcher failure preserves the original post and leaves settlement unmoved" $
      snd (stamp cause) == 0
        && cursor == 0
        && transaction == Nothing
        && not active
        && case posts of
          (original : _) -> from original == "runner" && to original == ["seat"] && body original == "TASK active watcher failure"
          [] -> False
    assert "active-watcher failure keeps an unaddressed notice and stdout/stderr tails" $
      case notices of
        [notice] ->
          channel notice == Nothing
            && "\"outcome\":\"failed\"" `T.isInfixOf` transcript
            && "\"exit_code\":3" `T.isInfixOf` transcript
            && "STDOUT-TAIL-MARKER" `T.isInfixOf` transcript
            && "STDERR-TAIL-MARKER" `T.isInfixOf` transcript
        _ -> False
    removePathForcibly root
