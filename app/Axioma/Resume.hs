{-# LANGUAGE OverloadedStrings #-}

-- | empty-reply drop (B2), empty-sentinel drop (B8), resume fix (B6).
module Axioma.Resume
  ( resumeTopic,
  )
where

import Axioma.Common (assert, closeBusDurable, mkPost, tempRoot)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (stamped, unframeStored)
import Circuit.Agent.Mark (Mark (..), markGlyph)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (openBus, postLocal, runSeatBus)
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cleanCliOut, cliDeliverTo, parseSessionId)
import Free.Agent.Delivery (TurnResult (..), awaitTurn)
import Free.Agent.Host (hostShard, mkHost)
import System.Directory (removePathForcibly)
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
    _ <- postLocal root (mkPost "human" ["echo"] (markGlyph Landed <> " done"))
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
    _ <- postLocal root (mkPost "human" ["silent"] (markGlyph Landed <> " done"))
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
    _ <- postLocal root (mkPost "human" ["hermes"] (markGlyph Landed <> " done"))
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
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing
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
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing
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
              cliScrub = id,
              cliStderr = StderrMerge,
              cliStderrTee = Nothing
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
