{-# LANGUAGE OverloadedStrings #-}

-- | Live Backend-seat settlement witnesses. These drive the built executable,
-- not the retired runAgentLoop, and use only local fake CLI processes.
module Axioma.Runtime
  ( runtimeTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Post (Post (..), PostId, deliversTo)
import Circuit.Shared (stamp, stamped)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, forM_, unless)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (postLocal, readLogStrict)
import Free.Agent.Bus.File (readCursor, readTransaction, writeCursor)
import Free.Agent.Bus.Heartbeat (HbState (..), readHeartbeat)
import Free.Agent.Mark (Mark (..), markGlyph)
import Free.Agent.Wake (quarantinePath, quarantinedPosts)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, removeDirectory, removePathForcibly)
import System.Environment (getEnv, getExecutablePath, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hClose, openFile)
import System.Posix.Files (setFileMode)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (ProcessHandle, StdStream (UseHandle), createProcess, getPid, proc, readProcessWithExitCode, spawnProcess, std_err, std_out, terminateProcess, waitForProcess)
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
runKimi = runKimiWith []

runKimiWith :: [String] -> FilePath -> Text -> IO (ExitCode, String, String)
runKimiWith extra root name = do
  bin <- seatBinary
  bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
    setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
    readProcessWithExitCode bin (kimiArgs root name <> extra) ""

runBus :: FilePath -> FilePath -> String -> [String] -> IO (ExitCode, String, String)
runBus bin root command args =
  readProcessWithExitCode bin (["bus", command, "--root", root] <> args) ""

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

waitForCursor :: FilePath -> Text -> PostId -> IO ()
waitForCursor root name target = go (400 :: Int)
  where
    go 0 = error ("timed out waiting for cursor " <> show target)
    go n = do
      current <- readCursor root name
      unless (current == target) (threadDelay 20_000 >> go (n - 1))

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

data B17Trial = B17Trial
  { b17Root :: FilePath,
    b17Calls :: Int,
    b17Rows :: Int,
    b17Replies :: Int,
    b17Notices :: Int,
    b17Exits :: [Int],
    b17Transaction :: Maybe PostId,
    b17Cursor :: PostId,
    b17Active :: Bool
  }

waitForText :: FilePath -> Text -> IO ()
waitForText path needle = go (400 :: Int)
  where
    go 0 = error ("timed out waiting for " <> path <> " to contain " <> T.unpack needle)
    go n = do
      exists <- doesFileExist path
      content <- if exists then TIO.readFile path else pure ""
      if needle `T.isInfixOf` content then pure () else threadDelay 20_000 >> go (n - 1)

spawnLogged :: FilePath -> [String] -> FilePath -> IO ProcessHandle
spawnLogged bin args stem = do
  out <- openFile (stem <> ".out") WriteMode
  err <- openFile (stem <> ".err") WriteMode
  (_, _, _, handle) <- createProcess (proc bin args) {std_out = UseHandle out, std_err = UseHandle err}
  hClose out
  hClose err
  pure handle

exitNumber :: ExitCode -> Int
exitNumber ExitSuccess = 0
exitNumber (ExitFailure n) = n

runB17Trial :: FilePath -> Int -> IO B17Trial
runB17Trial bin trial = do
  root <- tempRoot ("free-agent-b17-trial-" <> show trial)
  let participant = root </> "participant.sh"
      calls = root </> "calls"
      transcript = root </> ".transcripts" </> "seat.jsonl"
  TIO.writeFile (root </> "log.jsonl") ""
  TIO.writeFile (root </> "prompt.md") "B17 synchronized same-name witness."
  TIO.writeFile participant $
    "#!/bin/sh\nprintf 'call\\n' >> '"
      <> T.pack calls
      <> "'\nsleep 0.15\nprintf 'B17-REPLY\\n'\n"
  setFileMode participant 0o755
  writeCursor root "seat" 0
  let args =
        [ "cmd",
          "--root",
          root,
          "--name",
          "seat",
          "--cmd",
          participant,
          "--quiesce",
          "8"
        ]
  handles <- forM [1, 2 :: Int] $ \n -> spawnLogged bin args (root </> ("seat-" <> show n))
  forM_ [1, 2 :: Int] $ \n -> waitForText (root </> ("seat-" <> show n <> ".out")) "cursor:"
  _ <- postLocal root (mkPost "runner" ["seat"] "TASK synchronized B17 witness")
  let settled = do
        content <- TIO.readFile (root </> "log.jsonl")
        pure ("B17-REPLY" `T.isInfixOf` content || "ambiguous recovery" `T.isInfixOf` content)
      waitSettlement 0 = pure ()
      waitSettlement n = settled >>= \ok -> if ok then pure () else threadDelay 20_000 >> waitSettlement (n - 1)
  waitSettlement (400 :: Int)
  threadDelay 200_000
  _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
  exits <- forM handles $ \handle -> do
    exited <- timeout 10_000_000 (waitForProcess handle)
    case exited of
      Nothing -> do
        terminateProcess handle
        _ <- waitForProcess handle
        pure 124
      Just code -> pure (exitNumber code)
  callText <- callLog root
  transcriptExists <- doesFileExist transcript
  transcriptText <- if transcriptExists then TIO.readFile transcript else pure ""
  posts <- logPosts root
  cursor <- readCursor root "seat"
  transaction <- readTransaction root "seat"
  active <- doesFileExist (root </> ".active-seat")
  pure
    B17Trial
      { b17Root = root,
        b17Calls = length (T.lines callText),
        b17Rows = length (filter (not . T.null) (T.lines transcriptText)),
        b17Replies = length [() | p <- posts, from p == "seat", body p == "B17-REPLY"],
        b17Notices = length [() | p <- posts, from p == "seat", "ambiguous recovery" `T.isInfixOf` body p],
        b17Exits = exits,
        b17Transaction = transaction,
        b17Cursor = cursor,
        b17Active = active
      }

renderB17Trial :: Int -> B17Trial -> String
renderB17Trial trial result =
  "B17 trial="
    <> show trial
    <> " calls="
    <> show (b17Calls result)
    <> " rows="
    <> show (b17Rows result)
    <> " replies="
    <> show (b17Replies result)
    <> " notices="
    <> show (b17Notices result)
    <> " exits="
    <> show (b17Exits result)
    <> " transaction="
    <> show (b17Transaction result)
    <> " cursor="
    <> show (b17Cursor result)
    <> " active="
    <> show (b17Active result)
    <> " root="
    <> b17Root result

b17TrialClosed :: B17Trial -> Bool
b17TrialClosed result =
  b17Calls result == 1
    && b17Rows result == 1
    && b17Replies result == 0
    && b17Notices result == 1
    && b17Exits result == [0, 0]
    && b17Transaction result == Just 1
    && b17Cursor result == 3
    && not (b17Active result)

runtimeTopic :: IO ()
runtimeTopic = do
  putStrLn "live Backend-seat settlement"

  -- Default allow remains compatible: no wake restriction flag, one ordinary
  -- sender, one backend invocation.
  do
    root <- tempRoot "free-agent-runtime-wake-default"
    writeSeat root "seat" (successSource root "default-local")
    _ <- postLocal root (mkPost "ordinary" ["seat"] "default allow")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    (code, _, err) <- runKimi root "seat"
    calls <- T.lines <$> callLog root
    cursor <- readCursor root "seat"
    assert ("default wake policy exits cleanly" <> if code == ExitSuccess then "" else ": " <> err) $
      code == ExitSuccess && length calls == 1 && cursor == 2
    removePathForcibly root

  -- A restricted receiver quarantines a disallowed sender without invoking or
  -- claiming work. Polls stay quiet; an allowed post settles normally; explicit
  -- admission then delivers the old post once without moving the cursor back.
  do
    root <- tempRoot "free-agent-runtime-wake-admit"
    writeSeat root "seat" (successSource root "wake-local")
    bin <- seatBinary
    let transcript = root </> ".transcripts" </> "seat.jsonl"
        active = root </> ".active-seat"
        start =
          bracket (getEnv "PATH") (setEnv "PATH") $ \oldPath -> do
            setEnv "PATH" (root </> "bin" <> ":" <> oldPath)
            spawnLogged bin (kimiArgs root "seat" <> ["--wake-from", "allowed"]) (root </> "wake-seat")
        stop handle = terminateProcess handle >> waitForProcess handle >> pure ()
    bracket start stop $ \_ -> do
      waitForPath (root </> ".hb-seat")
      denied <- postLocal root (mkPost "blocked" ["seat"] "denied wake")
      waitForPath (quarantinePath root "seat")
      (inspectCode, inspectOut, inspectErr) <- runBus bin root "quarantined" ["seat"]
      calls <- callLog root
      posts <- logPosts root
      cursor <- readCursor root "seat"
      transaction <- readTransaction root "seat"
      transcriptExists <- doesFileExist transcript
      activeExists <- doesFileExist active
      heartbeat <- readHeartbeat root "seat"
      assert "denied wake is stored and inspectably quarantined without backend or settlement evidence" $
        inspectCode == ExitSuccess
          && "denied wake" `T.isInfixOf` T.pack inspectOut
          && T.null calls
          && length posts == 1
          && cursor == 0
          && transaction == Nothing
          && not transcriptExists
          && not activeExists
          && fmap (\(state, _, _) -> state) heartbeat == Just Wake
          && T.null (T.pack inspectErr)
      threadDelay 1_200_000
      quietCalls <- callLog root
      quietCursor <- readCursor root "seat"
      quietTranscript <- doesFileExist transcript
      quietActive <- doesFileExist active
      assert "repeated watcher scans leave denied mail quiet and unsettled" $
        T.null quietCalls && quietCursor == 0 && not quietTranscript && not quietActive

      _ <- postLocal root (mkPost "allowed" ["seat"] "allowed wake")
      waitForCursor root "seat" 2
      allowedCalls <- T.lines <$> callLog root
      stillQuarantined <- quarantinedPosts root "seat"
      assert "an allowed post delivers while denied mail stays quarantined" $
        length allowedCalls == 1 && map (snd . stamp) stillQuarantined == [snd (stamp denied)]

      (admitCode, _, admitErr) <- runBus bin root "admit" ["seat", show (snd (stamp denied))]
      waitForText transcript "\"post_id\":0"
      threadDelay 200_000
      admittedCalls <- T.lines <$> callLog root
      admittedCursor <- readCursor root "seat"
      admittedTransaction <- readTransaction root "seat"
      admittedTranscript <- TIO.readFile transcript
      admittedActive <- doesFileExist active
      admittedPosts <- logPosts root
      emptyQuarantine <- runBus bin root "quarantined" ["seat"]
      assert "explicit admission delivers the stored post once without cursor regression or raw publication" $
        admitCode == ExitSuccess
          && length admittedCalls == 2
          && admittedCursor == 2
          && admittedTransaction == Just 2
          && length (filter (not . T.null) (T.lines admittedTranscript)) == 2
          && not admittedActive
          && length admittedPosts == 2
          && emptyQuarantine == (ExitSuccess, "", "")
          && T.null (T.pack admitErr)

      (repeatCode, _, _) <- runBus bin root "admit" ["seat", show (snd (stamp denied))]
      threadDelay 1_200_000
      finalCalls <- T.lines <$> callLog root
      finalCursor <- readCursor root "seat"
      assert "already delivered mail cannot be admitted or delivered again" $
        repeatCode /= ExitSuccess && length finalCalls == 2 && finalCursor == 2
    removePathForcibly root

  -- One startup drain mixes denied and allowed mail. Only the allowed post is
  -- presented to the backend; the denied row remains quarantined beside it.
  do
    root <- tempRoot "free-agent-runtime-wake-mixed"
    writeSeat root "seat" (successSource root "mixed-local")
    _ <- postLocal root (mkPost "blocked" ["seat"] "mixed-denied")
    _ <- postLocal root (mkPost "allowed" ["seat"] "mixed-allowed")
    _ <- postLocal root (mkPost "allowed" ["seat"] (markGlyph Landed))
    (code, _, err) <- runKimiWith ["--wake-from", "allowed"] root "seat"
    calls <- T.lines <$> callLog root
    quarantined <- quarantinedPosts root "seat"
    cursor <- readCursor root "seat"
    transcript <- TIO.readFile (root </> ".transcripts" </> "seat.jsonl")
    active <- doesFileExist (root </> ".active-seat")
    assert ("mixed wake batch exits cleanly" <> if code == ExitSuccess then "" else ": " <> err) $
      code == ExitSuccess
        && ( case calls of
               [call] ->
                 "mixed-allowed" `T.isInfixOf` call
                   && not ("mixed-denied" `T.isInfixOf` call)
               _ -> False
           )
        && map (snd . stamp) quarantined == [0]
        && cursor == 3
        && length (filter (not . T.null) (T.lines transcript)) == 1
        && not active
    removePathForcibly root

  -- Channel and multi-sender input supplies no implicit audience or ancestry.
  -- A deliberate bus-post act chooses both; raw final prose remains local.
  do
    root <- tempRoot "free-agent-runtime-audience"
    bin <- seatBinary
    let source =
          T.pack (scriptPreamble root)
            <> T.pack
              ( unlines
                  [ show bin <> " bus post --root \"$FREE_AGENT_BUS_ROOT\" --from ops --to tony --to alex --body 'intentional report' --thread 1",
                    "printf 'To resume this session: kimi -r runtime-session\\n@tony: raw local reply\\n'"
                  ]
              )
    writeSeat root "ops" source
    _ <- postLocal root (mkPost "tony" [] "channel question") {channel = Just "ops"}
    _ <- postLocal root (mkPost "alex" ["ops"] "direct question")
    _ <- postLocal root (mkPost "runner" ["ops"] (markGlyph Landed))
    (code, _, err) <- runKimi root "ops"
    posts <- logPosts root
    let replies = [p | p <- posts, from p == "ops", not (null (to p))]
    transcript <- TIO.readFile (root </> ".transcripts" </> "ops.jsonl")
    assert ("channel/multi-sender seat exits cleanly" <> if code == ExitSuccess then "" else ": " <> err) $
      code == ExitSuccess
    assert "explicit bus post chooses its audience and ancestry; raw reply prose stays local" $
      case replies of
        [reply] ->
          channel reply == Nothing
            && to reply == ["tony", "alex"]
            && thread reply == [1]
            && body reply == "intentional report"
            && deliversTo reply ["tony"]
            && deliversTo reply ["alex"]
            && length posts == 4
            && not (any ((== "@tony: raw local reply") . body) posts)
            && "@tony: raw local reply" `T.isInfixOf` transcript
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
    assert "stale recovery rotates the sid and commits the recovered turn without a raw reply post" $
      sid == "new-sid"
        && cursor == 2
        && not (any (\p -> from p == "seat" && body p == "recovered") posts)
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

  -- Force transaction creation to fail after a quiet completed turn. Restart
  -- retains the settled evidence and exposes ambiguity without repeating the
  -- client turn or manufacturing a reply.
  do
    root <- tempRoot "free-agent-runtime-crash"
    writeSeat root "seat" (successSource root "raw-local-once")
    _ <- postLocal root (mkPost "human" ["seat"] "crash work")
    _ <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
    createDirectoryIfMissing False (root </> ".transaction-seat")
    (firstCode, _, _) <- runKimi root "seat"
    firstCalls <- T.lines <$> callLog root
    firstPosts <- logPosts root
    let firstReplies = [p | p <- firstPosts, from p == "seat", body p == "raw-local-once"]
    firstCursor <- readCursor root "seat"
    removeDirectory (root </> ".transaction-seat")
    (secondCode, _, _) <- runKimi root "seat"
    secondCalls <- T.lines <$> callLog root
    secondPosts <- logPosts root
    let secondReplies = [p | p <- secondPosts, from p == "seat", body p == "raw-local-once"]
    assert "forced post-turn crash leaves raw output local and no cursor" $
      firstCode /= ExitSuccess
        && length firstCalls == 1
        && null firstReplies
        && firstCursor == 0
    assert "ambiguous restart does not manufacture publication or repeat invocation" $
      secondCode == ExitSuccess
        && length secondCalls == 1
        && null secondReplies
        && any (\p -> from p == "seat" && "ambiguous recovery" `T.isInfixOf` body p) secondPosts
    removePathForcibly root

  -- Terminal evidence is not optional: if its append fails, settlement stops
  -- and the surviving active marker prevents a replay. Raw output stays local.
  do
    root <- tempRoot "free-agent-runtime-transcript-crash"
    writeSeat root "seat" (successSource root "raw-local")
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
    assert "failed transcript evidence leaves raw output local and a dead heartbeat" $
      firstCode /= ExitSuccess
        && length firstCalls == 1
        && not (any (\p -> from p == "seat" && body p == "raw-local") firstPosts)
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
        && not (any (\p -> from p == "seat" && body p == "recovered") posts)
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

  -- Two already-ready processes with one name race the same input. A trial
  -- fails on duplicate compute; raw output cannot publish or wake a receiver.
  -- Twenty bounded trials retain every per-trial root and measurement.
  do
    bin <- seatBinary
    oracle <- getExecutablePath
    putStrLn ("B17 oracle CLI: " <> bin)
    putStrLn ("B17 oracle axioma: " <> oracle)
    results <-
      forM [1 .. 20 :: Int] $ \trial -> do
        result <- runB17Trial bin trial
        putStrLn (renderB17Trial trial result)
        pure result
    assert "B17 synchronized same-name trials allow one invocation, row, no raw reply, and settlement" $
      all b17TrialClosed results
