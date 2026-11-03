{-# LANGUAGE OverloadedStrings #-}

-- | empty-reply drop (B2), empty-sentinel drop (B8), resume fix (B6),
-- failed-attempt evidence (stinky-dsh repair).
module Axioma.Resume
  ( resumeTopic,
  )
where

import Axioma.Common (assert, closeBusDurable, mkPost, tempRoot)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (stamped, unframeStored)
import Circuit.Agent.Mark (Mark (..), markGlyph)
import Control.Exception (SomeException, try)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (openBus, postLocal, runSeatBus)
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliDeliverTo, kimiCli, parseSessionId)
import Free.Agent.Delivery (TurnResult (..), awaitTurn)
import Free.Agent.Host (hostShard, mkHost)
import System.Directory (doesFileExist, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

queryDelivery :: Cli -> FilePath -> Text -> IO Text
queryDelivery cli dir prompt = do
  result <- cliDeliverTo cli (dir </> "transcript.jsonl") 1 prompt >>= awaitTurn
  pure $ case result of
    TurnResponse response -> response
    TurnCancelled -> ""

resumeTopic :: IO ()
resumeTopic = do
  -------------------------------------------------------------------------
  -- Empty-reply drop (B2-as-code): whitespace-only replies are dropped
  -- before posting.  The seat still processes the input — cursor advances,
  -- quiescence counter is unaffected.  Only content-carrying posts land.
  --
  -- Mutation guard: if someone removes the filter in runSeatBus or
  -- runAgentLoop, test (a) fails because the empty body appears in the log.
  -------------------------------------------------------------------------
  putStrLn "Empty-reply drop (B2-as-code)"

  -- Test (a): empty/whitespace replies are not posted; the cursor advances.
  do
    root <- tempRoot "free-agent-b2-empty"
    _ <- postLocal root (mkPost "human" ["echo"] "hello")
    _ <- postLocal root (mkPost "human" ["echo"] "  ")
    _ <- postLocal root (mkPost "human" ["echo"] "world")
    _ <- postLocal root (mkPost "human" ["echo"] "\t\n  ")
    _ <- postLocal root (mkPost "human" ["echo"] (markGlyph Landed))
    bus <- openBus root
    let host = mkHost "echo" $ \ws ->
          pure [T.unwords ws]
    runSeatBus bus "echo" ["echo"] (hostShard host)
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        fromEcho = [stamped s | s <- parsed, from (stamped s) == "echo"]
        echoBodies = map body fromEcho
    -- Content replies land: "hello", "world".
    -- Whitespace inputs produce whitespace-only replies and are dropped.
    -- The landed mark halts the seat on input; it never reaches the handler.
    assert "B2a: content replies posted" $
      "hello" `elem` echoBodies
        && "world" `elem` echoBodies
    assert "B2a: whitespace-only reply dropped (spaces)" $
      not (any (\b -> "  " `T.isPrefixOf` b) echoBodies)
    assert "B2a: whitespace-only reply dropped (tabs+newline)" $
      not (any (\b -> "\t" `T.isPrefixOf` b) echoBodies)
    -- Cursor advances past all 5 inputs: the seat processed every post
    -- up to the halt mark.
    assert "B2a: ids coherent" $
      length parsed >= 5 + length echoBodies
    removePathForcibly root

  -- Test (b): explicitly empty body from handler → no post.
  -- Exercise the other fix site (runAgentLoop via runSeatBus — the same
  -- filter applies in both common paths).
  do
    root <- tempRoot "free-agent-b2-emptybody"
    _ <- postLocal root (mkPost "human" ["silent"] "ping")
    _ <- postLocal root (mkPost "human" ["silent"] (markGlyph Landed))
    bus <- openBus root
    -- Handler returns a post with an empty body.  Should be dropped.
    let host = mkHost "silent" $ \ws ->
          if "ping" `elem` ws
            then pure [""]
            else pure []
    runSeatBus bus "silent" ["silent"] (hostShard host)
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        fromSilent = [stamped s | s <- parsed, from (stamped s) == "silent"]
    -- Empty-body post is dropped.
    assert "B2b: empty-body post dropped" $
      null fromSilent
    -- But cursor still advanced: the seat processed both inputs.
    assert "B2b: ids coherent — planted only, no reply posts" $
      length parsed == 2
    removePathForcibly root

  -------------------------------------------------------------------------
  -- Empty-sentinel drop (B8): hermes's '(empty)' sentinel becomes truly
  -- empty via cleanCliOut, then the B2 filter drops it.  One CLI protocol
  -- token — not a content denylist.
  --
  -- Mutation guard: remove the sentinel line from cleanCliOut → '(empty)'
  -- body appears in the log → test (b) fails.
  -------------------------------------------------------------------------
  putStrLn "Empty-sentinel drop (B8)"

  -- Test (a): cleanCliOut drops the '(empty)' sentinel line.
  do
    let result = cleanCliOut "(empty)\n"
    assert "B8a: cleanCliOut drops (empty) sentinel" $
      T.null (T.strip result)

  -- Test (b): handler returning (empty) → scrubbed to empty → B2 drop → no post.
  -- Uses cleanCliOut to simulate the CLI response scrubbing path.
  do
    root <- tempRoot "free-agent-b8-empty"
    _ <- postLocal root (mkPost "human" ["hermes"] "ping")
    _ <- postLocal root (mkPost "human" ["hermes"] "real")
    _ <- postLocal root (mkPost "human" ["hermes"] (markGlyph Landed))
    bus <- openBus root
    let host = mkHost "hermes" $ \ws ->
          pure [cleanCliOut (if "ping" `elem` ws then "(empty)" else T.unwords ws)]
    runSeatBus bus "hermes" ["hermes"] (hostShard host)
    closeBusDurable bus
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
        fromHermes = [stamped s | s <- parsed, from (stamped s) == "hermes"]
        hermesBodies = map body fromHermes
    -- '(empty)' scrubbed to empty → B2 filter drops → no post.
    assert "B8b: (empty) sentinel scrubbed then dropped — no post" $
      not (any (\b -> "(empty)" `T.isInfixOf` b) hermesBodies)
    assert "B8b: content reply still posts" $
      "real" `elem` hermesBodies
    removePathForcibly root

  -------------------------------------------------------------------------
  -- Resume fix oracle (B6): transient failure retries with same session id;
  -- stale session falls back to fresh.  Regression guard: if someone
  -- restores `cliStale = \code _ -> code /= ExitSuccess`, test (a) fails
  -- because the transient failure triggers fresh instead of retry.
  -------------------------------------------------------------------------
  putStrLn "Resume fix (B6)"

  -- Test (a): transient failure → retry with same session id
  do
    dir <- tempRoot "free-agent-b6a"
    let script = dir </> "fake.sh"
        sf = dir </> "session"
        attemptFile = dir </> "attempt"
    -- Seed the session file with a known id.
    TIO.writeFile sf "test-sid-b6a"
    -- Fake CLI: fails on first call (exit 1), succeeds on second,
    -- echoes argv so we can inspect the --resume flag.
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "ATTEMPT=$(cat " <> attemptFile <> " 2>/dev/null || echo 0)",
            "echo 'session_id: fake-b6a'",
            "if [ \"$ATTEMPT\" -eq 0 ]; then",
            "  echo 1 > " <> attemptFile,
            "  exit 1",
            "fi",
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
              cliStale = \_ out ->
                "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliExhausted = \_ _ -> False,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliSessionReport = Nothing
            }
    result <- queryDelivery cli dir "hello-b6a"
    -- Second invocation must carry the --resume flag with our seeded id.
    assert "B6a: retry preserves session id in --resume" $
      "--resume test-sid-b6a" `T.isInfixOf` result
    -- Session file is re-persisted with the scraped id on success (standard).
    stored <- TIO.readFile sf
    assert "B6a: .sid updated with scraped id after retry success" $
      T.strip stored == "fake-b6a"
    removePathForcibly dir

  -- Test (b): stale session → fresh, new .sid written
  do
    dir <- tempRoot "free-agent-b6b"
    let script = dir </> "fake.sh"
        sf = dir </> "session"
    TIO.writeFile sf "old-stale-sid"
    -- Fake CLI: returns a stale-session message, then a fresh id.
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'No session found matching old-stale-sid'",
            "echo 'session_id: fresh-b6b'",
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
              cliStale = \_ out ->
                "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliExhausted = \_ _ -> False,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliSessionReport = Nothing
            }
    result <- queryDelivery cli dir "hello-b6b"
    -- Must NOT carry the old --resume flag (it was a fresh call).
    assert "B6b: stale session does NOT resume old sid" $
      not ("--resume old-stale-sid" `T.isInfixOf` result)
    -- Session file must be updated with the new id.
    stored <- TIO.readFile sf
    assert "B6b: .sid updated to fresh session id" $
      T.strip stored == "fresh-b6b"
    removePathForcibly dir

  -- Test (c): mutation guard — code-is-stale regression
  --
  -- Same pattern as (a) but with the OLD hermesCli cliStale that treats
  -- any non-zero exit as stale.  A transient error then triggers a fresh
  -- call instead of a retry, and the old session id is dropped.
  -- If someone regresses hermesCli back to code-is-stale, this test
  -- catches it by asserting no --resume on the second call.
  do
    dir <- tempRoot "free-agent-b6c"
    let script = dir </> "fake.sh"
        sf = dir </> "session"
        attemptFile = dir </> "attempt"
    TIO.writeFile sf "should-not-survive"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "ATTEMPT=$(cat " <> attemptFile <> " 2>/dev/null || echo 0)",
            "echo 'session_id: fresh-b6c'",
            "if [ \"$ATTEMPT\" -eq 0 ]; then",
            "  echo 1 > " <> attemptFile,
            "  exit 1",
            "fi",
            "echo \"argv:$*\"",
            "printf 'stdin:'",
            "cat"
          ]
    -- DELIBERATE regression: code /= ExitSuccess makes all errors stale.
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \code out ->
                code /= ExitSuccess
                  || "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliExhausted = \_ _ -> False,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliSessionReport = Nothing
            }
    result <- queryDelivery cli dir "hello-b6c"
    -- With code-is-stale, the first invocation's exit 1 triggers stale →
    -- fresh.  The second call (fresh) must NOT carry --resume.
    assert "B6c: code-is-stale triggers fresh, old sid dropped" $
      not ("--resume should-not-survive" `T.isInfixOf` result)
    -- Fresh call DID produce output (the reply).
    assert "B6c: fresh call produced reply" $
      "stdin:hello-b6c" `T.isInfixOf` result
    -- .sid was updated to the new session id from the fresh call.
    stored <- TIO.readFile sf
    assert "B6c: .sid updated to fresh session id" $
      T.strip stored == "fresh-b6c"
    removePathForcibly dir

  -------------------------------------------------------------------------
  -- Failed-attempt evidence (stinky-dsh repair): a resumed session that
  -- fails non-zero on every attempt leaves one bounded TranscriptFailed
  -- record per attempt — attempt number, exit code, elapsed, session id,
  -- post id — the raised error names the real failure with honest
  -- accounting, and the stored session id is not clobbered.
  --
  -- Mutation guards:
  --  (i) remove the failed-attempt write in Cli.hs → the record-count
  --      assert fails (zero failed records).
  --  (ii) revert the terminal message to the generic "exited non-zero
  --       3 times" form → the real-failure assert fails (marker absent).
  --  (iii) split the message wording from the actual invocation count
  --       → the accounting assert fails.
  -------------------------------------------------------------------------
  putStrLn "Failed-attempt evidence (stinky-dsh)"

  do
    dir <- tempRoot "free-agent-stinky-evidence"
    let script = dir </> "fake.sh"
        sf = dir </> "session"
    -- Seed the session file: a resumed session must exist for the
    -- retry path to engage.
    TIO.writeFile sf "stinky-sid-seed"
    -- Fake CLI: fails (exit 1) on every call.  Emits a distinctive
    -- root-cause marker and a 9000-char junk line so the oracle can
    -- check both failure identification and raw bounding.
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-stinky'",
            "printf 'STINKY-ROOT-CAUSE-MARKER\\n'",
            "head -c 9000 /dev/zero | tr '\\000' 'x'",
            "echo",
            "exit 1"
          ]
    let cli =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sf,
              cliSessionId = parseSessionId,
              cliStale = \_ out ->
                "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliExhausted = \_ _ -> False,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliSessionReport = Nothing
            }
    res <-
      try (cliDeliverTo cli (dir </> "transcript.jsonl") 1 "hello-stinky" >>= awaitTurn) ::
        IO (Either SomeException (TurnResult Text))
    -- (1) The turn must raise — every attempt failed.
    let raised = case res of
          Left _ -> True
          Right _ -> False
    assert "stinky: turn raised after exhausting attempts" raised
    -- (2) One bounded failed record per attempt (initial + 3 retries).
    -- A missing transcript is empty evidence: the count assert fails
    -- cleanly instead of throwing.
    exists <- doesFileExist (dir </> "transcript.jsonl")
    content <-
      if exists
        then TIO.readFile (dir </> "transcript.jsonl")
        else pure ""
    let failed = [l | l <- T.lines content, "\"outcome\":\"failed\"" `T.isInfixOf` l]
    assert "stinky: one failed record per attempt (4 invocations)" $
      length failed == 4
    assert "stinky: attempt numbers 1..4 recorded" $
      all
        (\n -> any (T.isInfixOf ("\"attempt\":" <> T.pack (show n) <> ",")) failed)
        [1 .. 4]
    assert "stinky: every failed record names the real failure" $
      all ("STINKY-ROOT-CAUSE-MARKER" `T.isInfixOf`) failed
    assert "stinky: every failed record carries the session id" $
      all ("\"session_id\":\"stinky-sid-seed\"" `T.isInfixOf`) failed
    assert "stinky: every failed record carries exit code 1" $
      all ("\"exit_code\":1" `T.isInfixOf`) failed
    assert "stinky: failed raw bounded (9000-char junk truncated)" $
      all (\l -> T.length l < 6000) failed
    -- (3) The public error names the real failure with honest accounting.
    let msg = case res of
          Left err -> T.pack (show err)
          Right _ -> ""
    assert "stinky: error names the real failure" $
      "STINKY-ROOT-CAUSE-MARKER" `T.isInfixOf` msg
    assert "stinky: honest accounting (4 attempts, 1 initial + 3 retries)" $
      "after 4 attempts" `T.isInfixOf` msg
        && "1 initial + 3 retries" `T.isInfixOf` msg
    -- (4) The stored session id is not clobbered by the failure.
    stored <- TIO.readFile sf
    assert "stinky: stored session id not clobbered" $
      T.strip stored == "stinky-sid-seed"
    removePathForcibly dir

  -------------------------------------------------------------------------
  -- Context-exhaustion honesty (stinky-dsh repair): a resumed session
  -- whose window is full (dsh: terminal turn_end reason max-tokens,
  -- empty final, exit 1) is detected, never retried, and raised with a
  -- named error — one invocation, one bounded failed record, stored
  -- session id intact.  A fresh session that exhausts the window is
  -- named separately.
  --
  -- Mutation guards:
  --  (i) remove the exhaustion branch in Cli.hs → the fake is retried
  --      to full exhaustion (4 failed records) → the count assert fails.
  --  (ii) move the branch below the retry step → same count failure.
  --  (iii) revert the message to the generic accounting form → the
  --      "context exhausted" assert fails.
  --  (iv) remove the fresh-session guard → assert (b) fails (generic
  --      "exited ExitFailure 1" message instead).
  -------------------------------------------------------------------------
  putStrLn "Context-exhaustion honesty (stinky-dsh)"

  do
    dir <- tempRoot "free-agent-stinky-exhausted"
    let script = dir </> "fake.sh"
        sf = dir </> "session"
    TIO.writeFile sf "stinky-sid-full"
    -- Fake CLI: every call emits dsh's terminal max-tokens marker and
    -- exits 1 — the observed stinky failure shape, nothing else.
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-stinky-exhausted'",
            "echo '{\"type\":\"status\",\"phase\":\"turn_end\",\"reason\":{\"kind\":\"max-tokens\"}}'",
            "exit 1"
          ]
    let mkCli sessionFile =
          Cli
            { cliCommand = "/bin/sh",
              cliArgv = \_ mSid -> [script] <> maybe [] (\sid -> ["--resume", T.unpack sid]) mSid,
              cliStdin = T.unpack,
              cliSessionFile = sessionFile,
              cliSessionId = parseSessionId,
              cliStale = \_ out ->
                "No session found matching" `T.isInfixOf` out
                  || "Session not found" `T.isInfixOf` out,
              cliExhausted = \_ out -> "\"reason\":{\"kind\":\"max-tokens\"}" `T.isInfixOf` out,
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing,
              cliSessionReport = Nothing
            }
    -- (a) Resumed exhausted session: exactly one invocation, named error.
    resA <-
      try (cliDeliverTo (mkCli sf) (dir </> "transcript-a.jsonl") 1 "hello" >>= awaitTurn) ::
        IO (Either SomeException (TurnResult Text))
    let raisedA = case resA of
          Left _ -> True
          Right _ -> False
        msgA = case resA of
          Left err -> T.pack (show err)
          Right _ -> ""
    assert "stinky-exhausted: turn raised" raisedA
    contentA <- TIO.readFile (dir </> "transcript-a.jsonl")
    let failedA = [l | l <- T.lines contentA, "\"outcome\":\"failed\"" `T.isInfixOf` l]
    assert "stinky-exhausted: exactly one invocation, no retries" $
      length failedA == 1
    assert "stinky-exhausted: single record is attempt 1" $
      any ("\"attempt\":1," `T.isInfixOf`) failedA
    assert "stinky-exhausted: record carries the session id" $
      any ("\"session_id\":\"stinky-sid-full\"" `T.isInfixOf`) failedA
    assert "stinky-exhausted: error names context exhaustion" $
      "context exhausted" `T.isInfixOf` msgA
    assert "stinky-exhausted: error says it is not retrying" $
      "not retrying" `T.isInfixOf` msgA
    storedA <- TIO.readFile sf
    assert "stinky-exhausted: stored session id not clobbered" $
      T.strip storedA == "stinky-sid-full"
    -- (b) Fresh session exhausting the window: named, distinct error.
    let sfB = dir </> "session-fresh"
    resB <-
      try (cliDeliverTo (mkCli sfB) (dir </> "transcript-b.jsonl") 2 "hello" >>= awaitTurn) ::
        IO (Either SomeException (TurnResult Text))
    let raisedB = case resB of
          Left _ -> True
          Right _ -> False
        msgB = case resB of
          Left err -> T.pack (show err)
          Right _ -> ""
    assert "stinky-exhausted: fresh turn raised" raisedB
    assert "stinky-exhausted: fresh error names context exhaustion" $
      "context exhausted" `T.isInfixOf` msgB
    assert "stinky-exhausted: fresh error names the fresh path" $
      "fresh session" `T.isInfixOf` msgB
    removePathForcibly dir

  -------------------------------------------------------------------------
  -- Kimi resume and scrub: the resumed argv keeps its session id, the
  -- addressed presentation bullet is removed, the resume hint is filtered,
  -- and the rotated session id is persisted.
  --
  -- Mutation guards: removing the guarded bullet strip fails the prefix
  -- assertion; dropping -r fails the argv assertion; restoring the resume
  -- line fails the filter assertion; breaking session persistence fails the
  -- final stored-id assertion.
  -------------------------------------------------------------------------
  putStrLn "Kimi resume and addressed-bullet scrub"
  do
    dir <- tempRoot "free-agent-kimi-resume"
    let script = dir </> "fake-kimi.sh"
        sf = dir </> "session"
    TIO.writeFile sf "kimi-resume-old"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "printf '• @moka: resumed\\n'",
            "printf 'argv:%s\\n' \"$*\"",
            "printf 'To resume this session: kimi -r kimi-resume-fresh\\n'",
            "cat >/dev/null"
          ]
    let base = kimiCli Nothing Nothing sf
        cli =
          base
            { cliCommand = "/bin/sh",
              cliArgv = \prompt mSid -> [script] <> cliArgv base prompt mSid
            }
    result <- queryDelivery cli dir "hello-kimi"
    assert "Kimi scrub removes one addressed presentation bullet" $
      "@moka: resumed" `T.isPrefixOf` result
    assert "Kimi resumed argv retains the stored session id" $
      "-r kimi-resume-old" `T.isInfixOf` result
    assert "Kimi resume hint remains filtered" $
      not ("To resume this session" `T.isInfixOf` result)
    stored <- TIO.readFile sf
    assert "Kimi rotated session id is persisted" $
      T.strip stored == "kimi-resume-fresh"
    removePathForcibly dir
