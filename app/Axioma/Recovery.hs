{-# LANGUAGE OverloadedStrings #-}

-- | B16: a resume failure is not a turn failure.
--
-- Three failure classes, three different marks on disk. A turn that fails is
-- retried once, then skipped as a committed decision — transaction marker,
-- then cursor — so the seat keeps serving new mail; its notice says what disk
-- knows about the turn. A failure of the delivery environment (the CLI's
-- stored session will not resume) is not a turn at all: no turn ran, so the
-- batch is not committed, the cursor does not move, and the batch waits for
-- the next launch to re-deliver it. The witness for that distinction is the
-- pair of runs below: the first fails to resume and leaves the batch unread;
-- the second re-delivers exactly the same post ids.
--
-- The notice is evidence for a supervisor, not mail. It is unaddressed, so no
-- seat can spend a turn answering it — the #707-#712 chain was six posts of
-- overhead produced by notices that woke their senders.
module Axioma.Recovery
  ( recoveryTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Parser.Framing (decodeRow)
import Circuit.Post (Post (..), PostId)
import Circuit.Shared (stamp, stamped)
import Free.Agent.Mark (Mark (..), markGlyph)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (unless, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Agent.Runner (runAgentLoop)
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.File (readCursor, readTransaction, writeCursor)
import Free.Agent.Cli (Cli (..), StderrPolicy (..), cliDeliverTo, parseSessionId)
import Free.Agent.Delivery
  ( DeliveryFailure (..),
    DeliveryFailureKind (..),
    Turn (..),
    TurnMetrics (..),
    TurnResult (..),
    awaitTurn,
    newDeliveryGrant,
  )
import System.Directory (doesDirectoryExist, removePathForcibly)
import System.FilePath ((</>))
import System.Timeout (timeout)

-- | The measured resume failure, verbatim from the 15 Nov field run.
resumeMessage :: Text
resumeMessage =
  "cliQuery: hermes resumed session failed after 4 attempts (1 initial + 3 retries); last attempt 4 exited ExitFailure 2; session deadbeef; last output: "

-- | A turn that settles nothing: the shape every witness uses as a stand-in
-- whenever the turn itself is not the subject.
settled :: Turn [Post Text Text]
settled =
  Turn
    { awaitTurn = pure (TurnResponse []),
      cancelTurn = pure (),
      turnMetrics = pure (TurnMetrics False False 0)
    }

-- | Every post in the root's log, in log order.
logPosts :: FilePath -> IO [Post Text Text]
logPosts root = do
  content <- TIO.readFile (root <> "/log.jsonl")
  pure [stamped s | l <- T.lines content, Right s <- [decodeRow l]]

recoveryTopic :: IO ()
recoveryTopic = do
  putStrLn "B16: a resume failure is not a turn failure"

  -------------------------------------------------------------------------
  -- R0: the classification itself, at the CLI. The runner can only keep the
  -- two failure classes apart because the CLI raises one and not the other:
  -- an exhausted resume is 'ResumeFailed' (no turn ran), an unlaunchable
  -- backend is 'SpawnFailed'. Both are delivery-environment failures, and
  -- both keep their failed-attempt records and the stored session id.
  --
  -- Mutation guard: revert either raise in Cli.hs to the plain `fail` form
  -- and the assert fails — the runner would see a turn failure and skip.
  -------------------------------------------------------------------------
  do
    dir <- tempRoot "free-agent-b16-cli"
    let script = dir </> "fake.sh"
        sf = dir </> "session"
    TIO.writeFile sf "b16-sid-seed"
    TIO.writeFile script $
      T.pack $
        unlines
          [ "#!/bin/sh",
            "echo 'session_id: fake-b16'",
            "echo 'custom rate-limited'",
            "cat >/dev/null",
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
      try (cliDeliverTo cli (dir </> "transcript.jsonl") 1 "hello" >>= awaitTurn) ::
        IO (Either SomeException (TurnResult Text))
    assert "R0a an exhausted resume is raised as a delivery failure, not a turn failure" $
      case res of
        Left e -> case fromException e of
          Just (DeliveryFailure ResumeFailed detail) ->
            "resumed session failed after 4 attempts" `T.isInfixOf` detail
              && "custom rate-limited" `T.isInfixOf` detail
          _ -> False
        Right _ -> False
    content <- TIO.readFile (dir </> "transcript.jsonl")
    let failed = [l | l <- T.lines content, "\"outcome\":\"failed\"" `T.isInfixOf` l]
    assert "R0b every failed attempt is still recorded" $
      length failed == 4
    let missing = cli {cliCommand = "/nonexistent/b16-backend"}
    resSpawn <-
      try (cliDeliverTo missing (dir </> "transcript-spawn.jsonl") 1 "hello" >>= awaitTurn) ::
        IO (Either SomeException (TurnResult Text))
    assert "R0c an unlaunchable backend is raised as a spawn failure" $
      case resSpawn of
        Left e -> case fromException e of
          Just (DeliveryFailure SpawnFailed detail) ->
            "could not be started" `T.isInfixOf` detail
          _ -> False
        Right _ -> False
    stored <- TIO.readFile sf
    assert "R0d the stored session id survives an exhausted resume" $
      T.strip stored == "b16-sid-seed"
    removePathForcibly dir

  -------------------------------------------------------------------------
  -- R1: the environment cannot resume. Three deliveries fail, the seat
  -- stands down, and nothing about the batch has been committed.
  -------------------------------------------------------------------------
  root <- tempRoot "free-agent-b16-resume"
  writeCursor root "seat" 0
  p1 <- postLocal root (mkPost "human" ["seat"] "first task")
  p2 <- postLocal root (mkPost "human" ["seat"] "second task")
  attempts <- newIORef (0 :: Int)
  done <- newEmptyMVar
  grant <- newDeliveryGrant 100
  let unresumable _ = do
        modifyIORef' attempts (+ 1)
        throwIO (DeliveryFailure ResumeFailed resumeMessage)
  _ <- forkIO (runAgentLoop "seat" ["seat"] root Nothing grant unresumable >> putMVar done ())
  exited <- timeout 60000000 (takeMVar done)
  tries <- readIORef attempts
  cursorAfterFailure <- readCursor root "seat"
  markerAfterFailure <- readTransaction root "seat"
  posts <- logPosts root
  let notices = [p | p <- posts, from p == "seat"]
      noticeBody = T.intercalate " || " (map body notices)
  assert "R1a a resume failure is re-presented to its own ceiling, then the seat stands down" $
    isJust exited && tries == 3
  assert "R1b the resume failure does not take the skip path: cursor unmoved, no transaction marker" $
    cursorAfterFailure == 0 && markerAfterFailure == Nothing
  assert "R1c the notice names every post id in the batch, not only the head" $
    any (T.isInfixOf (T.pack (show (snd (stamp p1)))) . body) notices
      && any (T.isInfixOf (T.pack (show (snd (stamp p2)))) . body) notices
  assert "R1d the notice names the failure class and the ceiling it reached" $
    T.isInfixOf "resume failed" noticeBody
      && T.isInfixOf "attempt 3 of 3" noticeBody
      && T.isInfixOf "cursor is unmoved" noticeBody
  assert "R1e the notice is unaddressed, so no seat spends a turn answering it" $
    all (null . to) notices

  -------------------------------------------------------------------------
  -- R1 restart: the same batch is still there and is delivered to a working
  -- turn. The cursor only moves because a turn answered.
  -------------------------------------------------------------------------
  seen <- newIORef ([] :: [PostId])
  answered <- newIORef False
  done' <- newEmptyMVar
  grant' <- newDeliveryGrant 100
  halt <- postLocal root (mkPost "runner" ["seat"] (markGlyph Landed))
  let healthy batch = do
        modifyIORef' seen (++ map (snd . stamp) batch)
        pure
          settled
            { awaitTurn = do
                modifyIORef' answered (const True)
                pure (TurnResponse [mkPost "seat" ["human"] "answered"])
            }
  _ <- forkIO (runAgentLoop "seat" ["seat"] root Nothing grant' healthy >> putMVar done' ())
  exited' <- timeout 30000000 (takeMVar done')
  redelivered <- readIORef seen
  answeredTurn <- readIORef answered
  cursorAfterRestart <- readCursor root "seat"
  markerAfterRestart <- readTransaction root "seat"
  assert "R1f a restart re-delivers exactly the batch the resume failure left behind" $
    isJust exited' && redelivered == [snd (stamp p1), snd (stamp p2)] && answeredTurn
  assert "R1g the re-delivered batch is committed: transaction then cursor past the halt post" $
    markerAfterRestart == Just (snd (stamp p2) + 1) && cursorAfterRestart == snd (stamp halt) + 1

  exists <- doesDirectoryExist root
  unless (not exists) (removePathForcibly root)

  -------------------------------------------------------------------------
  -- R2: the turn itself fails. That is still the skip path: committed, so
  -- the seat keeps serving — but its notice carries what disk says.
  -------------------------------------------------------------------------
  root2 <- tempRoot "free-agent-b16-turn"
  writeCursor root2 "seat" 0
  q1 <- postLocal root2 (mkPost "human" ["seat"] "first task")
  q2 <- postLocal root2 (mkPost "human" ["seat"] "second task")
  halt2 <- postLocal root2 (mkPost "runner" ["seat"] (markGlyph Landed))
  tried <- newIORef (0 :: Int)
  done2 <- newEmptyMVar
  grant2 <- newDeliveryGrant 100
  let broken _ = do
        modifyIORef' tried (+ 1)
        throwIO (userError "backend refused the turn")
  _ <- forkIO (runAgentLoop "seat" ["seat"] root2 Nothing grant2 broken >> putMVar done2 ())
  exited2 <- timeout 30000000 (takeMVar done2)
  tries2 <- readIORef tried
  cursor2 <- readCursor root2 "seat"
  marker2 <- readTransaction root2 "seat"
  posts2 <- logPosts root2
  let notices2 = [p | p <- posts2, from p == "seat"]
      noticeBody2 = T.intercalate " || " (map body notices2)
  assert "R2a a failed turn is retried once, then skipped" $
    isJust exited2 && tries2 == 2
  assert "R2b the skip is a committed decision: transaction marker and cursor past the batch" $
    marker2 == Just (snd (stamp q2) + 1) && cursor2 == snd (stamp halt2) + 1
  assert "R2c the skip notice names every batch id and is unaddressed" $
    any (T.isInfixOf (T.pack (show (snd (stamp q1)))) . body) notices2
      && any (T.isInfixOf (T.pack (show (snd (stamp q2)))) . body) notices2
      && all (null . to) notices2
  assert "R2d the skip notice reports that no turn published and no transcript row exists" $
    T.isInfixOf "evidence published none; transcript none" noticeBody2

  exists2 <- doesDirectoryExist root2
  unless (not exists2) (removePathForcibly root2)

  -------------------------------------------------------------------------
  -- R3: the two states that looked identical in the field. A turn that
  -- published mid-batch and then died cites the post it published; a turn
  -- that never ran says silence. Same skip, different evidence.
  -------------------------------------------------------------------------
  root3 <- tempRoot "free-agent-b16-published"
  writeCursor root3 "seat" 0
  _ <- postLocal root3 (mkPost "human" ["seat"] "first task")
  _ <- postLocal root3 (mkPost "human" ["seat"] "second task")
  _ <- postLocal root3 (mkPost "runner" ["seat"] (markGlyph Landed))
  done3 <- newEmptyMVar
  grant3 <- newDeliveryGrant 100
  let publishesThenDies batch = do
        let ids = map (snd . stamp) batch
        void $ postLocal root3 (mkPost "seat" ["human"] "half a reply") {thread = ids}
        throwIO (userError "child died after publishing")
  _ <- forkIO (runAgentLoop "seat" ["seat"] root3 Nothing grant3 publishesThenDies >> putMVar done3 ())
  exited3 <- timeout 30000000 (takeMVar done3)
  posts3 <- logPosts root3
  let notices3 = [p | p <- posts3, from p == "seat", T.isInfixOf "failed on posts" (body p)]
  assert "R3a a turn that published then died leaves its publication in the notice evidence" $
    isJust exited3 && case notices3 of
      [n] ->
        T.isInfixOf "evidence published " (body n)
          && not (T.isInfixOf "evidence published none" (body n))
      _ -> False
  exists3 <- doesDirectoryExist root3
  unless (not exists3) (removePathForcibly root3)
