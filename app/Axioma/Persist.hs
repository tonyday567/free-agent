{-# LANGUAGE OverloadedStrings #-}

-- | watermark persister (B1-B3), anti-pollution (B4), stamping daemon.
module Axioma.Persist
  ( persistTopic,
  )
where

import Axioma.Common (assert, closeBusDurable, mkPost)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (stamp, stamped, unLog, unframeStored)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM (atomically, orElse)
import Data.Either (isLeft, isRight)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus
  ( awaitDurable,
    awaitQuiescent,
    awaitSince,
    busImage,
    closeBus,
    openBus,
    postLocal,
    readSince,
    scribeIO,
  )
import Free.Agent.Bus.Daemon (fifoPath, postViaDaemon, runDaemon)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (getExecutablePath, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (AppendMode), hFlush, withFile)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

persistTopic :: IO ()
persistTopic = do
  -------------------------------------------------------------------------
  -- Watermark persister (B1-B3)
  --
  -- The ack is off the agent path: durability is a predicate over the
  -- watermark and persister health, never a takeTMVar handoff. Death is
  -- content — any persister exit writes Failed, and every wait answers
  -- Left instead of hanging.
  -------------------------------------------------------------------------
  putStrLn "watermark persister"

  -- B1: an injected append failure turns every wait Left; nothing hangs.
  -- (Counterexample under the old code: the takeTMVar ack blocked
  -- forever — these assertions could not even run.)
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-b1-failed"
        path = root </> "log.jsonl"
    removePathForcibly root
    createDirectoryIfMissing True root
    bus <- openBus root
    _ <- scribeIO bus (mkPost "human" ["seat"] "healthy")
    r0 <- atomically (awaitDurable bus 0)
    assert "B1 happy path: post 0 durable before injection" $ isRight r0
    -- orElse composition, escape half: a still-pending wait retries, so a
    -- competing branch wins. (Catches a durable-check deleted: the first
    -- branch would answer Left spuriously.)
    rEsc <- atomically (awaitDurable bus 99 `orElse` pure (Right ()))
    assert "B1 orElse: a pending wait retries, a competing branch wins" $ isRight rEsc
    -- Inject an append failure: the log path becomes a directory.
    removeFile path
    createDirectoryIfMissing True path
    _ <- scribeIO bus (mkPost "human" ["seat"] "doomed")
    r1 <- atomically (awaitDurable bus 1)
    rOld <- atomically (awaitDurable bus 0)
    rq <- atomically (awaitQuiescent bus)
    assert "B1 lost ack: the doomed post's wait answers Left" $ isLeft r1
    assert "B1 lost ack: failure dominates even a durable id" $ isLeft rOld
    assert "B1 lost ack: quiescence answers Left, never hangs" $ isLeft rq
    -- orElse composition, answer half: failure answers Left instead of
    -- retrying and falling through to the competing branch. (Catches a
    -- Failed-case turned into retry: the second branch would yield Right.)
    rAlt <- atomically (awaitDurable bus 1 `orElse` pure (Right ()))
    assert "B1 orElse: failure answers Left instead of falling through" $ isLeft rAlt
    closeBus bus
    removePathForcibly root

  -- B2: undurable delivery — no reader can react to a post the persister
  -- never wrote. Deterministic half: readSince excludes the ghost.
  -- Death-detection half: awaitSince cannot produce it within a bound.
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-b2-undurable"
        path = root </> "log.jsonl"
    removePathForcibly root
    createDirectoryIfMissing True root
    bus <- openBus root
    _ <- scribeIO bus (mkPost "human" ["seat"] "durable")
    _ <- atomically (awaitDurable bus 0)
    removeFile path
    createDirectoryIfMissing True path
    _ <- scribeIO bus (mkPost "human" ["seat"] "ghost")
    seen <- atomically (readSince bus ["seat"] 0)
    assert "B2 undurable: readSince never surfaces the ghost" $
      map (body . stamped) seen == ["durable"]
    timedOut <- timeout 200_000 (atomically (awaitSince bus ["seat"] 1))
    assert "B2 undurable: awaitSince cannot deliver the ghost" $
      timedOut == Nothing
    closeBus bus
    removePathForcibly root
    -- Positive half: after a successful persist, delivery flows.
    let root2 = tmp </> "free-agent-b2-recovered"
    removePathForcibly root2
    createDirectoryIfMissing True root2
    bus2 <- openBus root2
    _ <- scribeIO bus2 (mkPost "human" ["seat"] "landed")
    _ <- atomically (awaitDurable bus2 0)
    got <- atomically (awaitSince bus2 ["seat"] 0)
    assert "B2 after a successful persist, the post delivers" $
      map (body . stamped) got == ["landed"]
    closeBusDurable bus2
    removePathForcibly root2

  -- B3: id agreement — scribe ids (image length) and postLocal ids (file
  -- line count) share one root without colliding. The persister
  -- reconciles external growth: pulls the external posts into the image
  -- and renumbers its invisible not-yet-durable tail past them, so ids
  -- stay file-positional on every interleaving.
  do
    let positional ids =
          let n = length ids
           in ids == map fromIntegral ([0 .. n - 1] :: [Int])
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-b3-ids"
        path = root </> "log.jsonl"
        agree bus = do
          rq <- atomically (awaitQuiescent bus)
          assert "B3 persister alive" $ isRight rq
          -- Reconciliation of trailing external growth rides the
          -- persister's one-second tick (wave H): wait (bounded) until
          -- the image has pulled the file in, then compare.
          let waitReconciled :: Int -> IO Bool
              waitReconciled 0 = pure False
              waitReconciled n = do
                imgLen <- atomically $ length . unLog <$> busImage bus
                fileLen <- length . T.lines <$> TIO.readFile path
                if imgLen == fileLen
                  then pure True
                  else threadDelay 100_000 >> waitReconciled (n - 1)
          reconciled <- waitReconciled 100
          assert "B3 image reconciled with the file" reconciled
          img <- atomically (busImage bus)
          content <- TIO.readFile path
          let parsed = mapMaybe (unframeStored @Text) (T.lines content)
              imgIds = map (snd . stamp) (unLog img)
              fileIds = map (snd . stamp) parsed
          assert "B3 image ids are file-positional and duplicate-free" $
            positional imgIds
          assert "B3 file ids equal image ids" $
            fileIds == imgIds
          assert "B3 image content equals file content" $
            map stamped (unLog img) == map stamped parsed
    removePathForcibly root
    createDirectoryIfMissing True root
    _ <- postLocal root (mkPost "human" ["seat"] "seed")
    bus <- openBus root
    -- Deterministic reconcile: external growth lands while the
    -- watermark is behind, then a scribe makes the persister look.
    _ <- postLocal root (mkPost "cli" ["seat"] "external-1")
    _ <- postLocal root (mkPost "cli" ["seat"] "external-2")
    _ <- scribeIO bus (mkPost "seat" ["human"] "reply")
    agree bus
    -- Racing interleave burst: every postLocal id depends on whether the
    -- persister flushed first; the contract holds on either order.
    traverse_
      ( \i -> do
          _ <- scribeIO bus (mkPost "seat" ["human"] ("s" <> T.pack (show i)))
          _ <- postLocal root (mkPost "cli" ["seat"] ("p" <> T.pack (show i)))
          pure ()
      )
      [0 .. 9 :: Int]
    agree bus
    closeBus bus
    removePathForcibly root

  -------------------------------------------------------------------------
  -- bus post anti-pollution (B4)
  --
  -- FREE_AGENT_BUS_ROOT env var + refuse when no log.jsonl exists.
  -------------------------------------------------------------------------
  putStrLn "bus post anti-pollution"

  do
    tmp <- getTemporaryDirectory
    let busRoot = tmp </> "free-agent-b4-bus"
        badRoot = tmp </> "free-agent-b4-nobus"
    removePathForcibly busRoot
    removePathForcibly badRoot
    createDirectoryIfMissing True busRoot
    createDirectoryIfMissing True badRoot
    -- Create a real bus with a log.jsonl
    _ <- postLocal busRoot (mkPost "test" [] "seed")
    -- Test 1: postLocal on a real bus works
    stored <- postLocal busRoot (mkPost "verify" [] "works")
    assert "post to an existing bus succeeds" $
      snd (stamp stored) == 1
    -- Test 2: postLocal on a non-bus directory creates one (existing behavior)
    stored2 <- postLocal badRoot (mkPost "verify" [] "also-works")
    assert "postLocal creates bus when absent (library, not CLI)" $
      snd (stamp stored2) == 0
    removePathForcibly busRoot
    removePathForcibly badRoot

  -- CLI-level test: the binary refuses when no log.jsonl and no env var
  do
    tmp <- getTemporaryDirectory
    let nobusDir = tmp </> "free-agent-b4-cli-nobus"
        busDir = tmp </> "free-agent-b4-cli-bus"
    removePathForcibly nobusDir
    removePathForcibly busDir
    createDirectoryIfMissing True nobusDir
    createDirectoryIfMissing True busDir
    -- Create a bus with log.jsonl for the env var test
    _ <- postLocal busDir (mkPost "seed" [] "init")
    -- Find the free-agent binary relative to the axioma binary
    myPath <- takeDirectory <$> getExecutablePath
    let freeAgentBin =
          myPath </> ".." </> ".." </> ".." </> "free-agent" </> "build" </> "free-agent" </> "free-agent"
    -- Test: no env var, no log.jsonl → exit 1
    unsetEnv "FREE_AGENT_BUS_ROOT"
    (code1, out1, _) <-
      readProcessWithExitCode
        freeAgentBin
        ["bus", "post", "--from", "test", "--to", "test", "--body", "should-fail"]
        ""
    assert "CLI post from non-bus dir exits 1" $
      code1 == ExitFailure 1
    assert "CLI post error message names the dir" $
      T.isInfixOf "no log.jsonl" (T.pack out1)
    -- Test: FREE_AGENT_BUS_ROOT → succeeds
    setEnv "FREE_AGENT_BUS_ROOT" busDir
    (code2, out2, _) <-
      readProcessWithExitCode
        freeAgentBin
        ["bus", "post", "--from", "test", "--to", "test", "--body", "env-var-works"]
        ""
    unsetEnv "FREE_AGENT_BUS_ROOT"
    assert "CLI post with FREE_AGENT_BUS_ROOT exits 0" $
      code2 == ExitSuccess
    let parsed = unframeStored @Text (T.strip (T.pack out2))
    assert "CLI post lands on the correct bus" $
      case parsed of
        Just s -> snd (stamp s) >= 0 && body (stamped s) == "env-var-works"
        Nothing -> False
    removePathForcibly nobusDir
    removePathForcibly busDir
  -------------------------------------------------------------------------
  -- Stamping daemon (post-horn): single-writer office over bus.fifo
  --
  -- Mutation guard: if the daemon races with postLocal or assigns colliding
  -- ids, the total-order assertion fails. If it drops a post, the body-set
  -- equality fails. If malformed input kills it, the garbage-survivor test
  -- times out.
  -------------------------------------------------------------------------
  putStrLn "Stamping daemon"

  -- Test (a): concurrent posters produce a contiguous total order.
  do
    tmp <- getTemporaryDirectory
    let posters = ["a", "b", "c", "d"]
        postsPerPoster = 5
        expectedBodies =
          [name <> "-" <> T.pack (show i) | name <- posters, i <- [1 .. postsPerPoster]]
        trialRoot n = tmp </> ("free-agent-daemon-order-" <> show (n :: Int))
        runTrial n = do
          let root = trialRoot n
          removePathForcibly root
          createDirectoryIfMissing True root
          tid <- forkIO (runDaemon @Text root)
          threadDelay 100_000
          mvars <-
            mapM
              ( \name -> do
                  mv <- newEmptyMVar
                  _ <- forkIO $ do
                    rs <-
                      mapM
                        ( \i ->
                            let body = name <> "-" <> T.pack (show (i :: Int))
                             in postViaDaemon root (mkPost name [name] body)
                        )
                        [1 .. postsPerPoster]
                    putMVar mv rs
                  pure mv
              )
              posters
          collected <- mapM takeMVar mvars
          killThread tid
          let receipts = concat collected
          content <- TIO.readFile (root </> "log.jsonl")
          let parsed = mapMaybe (unframeStored @Text) (T.lines content)
              ids = map (snd . stamp) parsed
              bodies = map (body . stamped) parsed
          assert ("daemon total order: contiguous ids from 0 (trial " <> show n <> ")") $
            sort ids == [0 .. fromIntegral (length posters * postsPerPoster - 1)]
          assert ("daemon total order: every body present exactly once (trial " <> show n <> ")") $
            sort bodies == sort expectedBodies
          assert ("daemon total order: receipts equal log entries (trial " <> show n <> ")") $
            sort (map (snd . stamp) receipts) == sort ids
          assert ("daemon total order: timestamps monotone by id (trial " <> show n <> ")") $
            and (zipWith (<=) (map (fst . stamp) parsed) (drop 1 (map (fst . stamp) parsed)))
          pure root
    roots <- mapM runTrial [1 .. 5]
    mapM_ removePathForcibly roots

  -- Test (b): malformed FIFO input is skipped; the daemon survives.
  do
    tmp <- getTemporaryDirectory
    let root = tmp </> "free-agent-daemon-garbage"
    removePathForcibly root
    createDirectoryIfMissing True root
    tid <- forkIO (runDaemon @Text root)
    threadDelay 100_000
    -- Send a line that is not a valid bare post.
    withFile (fifoPath root) AppendMode $ \h -> do
      TIO.hPutStrLn h "not a valid post"
      hFlush h
    -- A valid post afterwards must still be stamped.
    stored <- postViaDaemon root (mkPost "good" [] "ok")
    killThread tid
    assert "daemon survives garbage input" $
      body (stamped stored) == "ok"
    assert "daemon garbage survivor gets id 0" $
      snd (stamp stored) == 0
    content <- TIO.readFile (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
    assert "daemon log contains exactly the good post" $
      length parsed == 1
    removePathForcibly root
