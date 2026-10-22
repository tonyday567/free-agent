{-# LANGUAGE OverloadedStrings #-}

-- | lifecycle hygiene (wave H): the log lock, the stamping daemon, and
-- death honesty. L1-L5.
module Axioma.Lifecycle
  ( lifecycleTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (Post (..), ioShard)
import Circuit.Agent.Framing (stamp, stamped, unLog, unframeStored)
import Control.Concurrent (forkFinally, forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, displayException, try)
import Data.Either (isLeft)
import Data.List (nub, sort)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (diffUTCTime, getCurrentTime)
import Free.Agent.Bus
  ( awaitQuiescent,
    awaitSince,
    busImage,
    closeBus,
    openBus,
    postLocal,
    readFileStrict,
    runSeatBus,
    scribeIO,
  )
import Free.Agent.Bus.Daemon (ensureFifo, fifoPath, postViaDaemon, runDaemon)
import System.Directory (removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.Mem (performGC)
import System.Posix.Files (getFileStatus, isNamedPipe)
import System.Timeout (timeout)

lifecycleTopic :: IO ()
lifecycleTopic = do
  -------------------------------------------------------------------------
  -- L1: lock stress — forked writers hammer postLocal against a live
  -- persister while a feeder keeps the persistLoop inside the lock.
  --
  -- Pre-fix (blocking withFileLock, non-threaded RTS) a waiter wedged in
  -- the flock FFI while the holder was descheduled deadlocked the runtime
  -- permanently; the in-test timeout cannot fire once the wedge lands, so
  -- the red evidence is an externally-killed binary (observed: wedged on
  -- the 3rd of 15 attempts against the pre-fix build).
  --
  -- Mutation guard: a lock regression re-wedges under the same hammer.
  -------------------------------------------------------------------------
  putStrLn "lock stress"

  do
    root <- tempRoot "free-agent-l1-lock"
    let path = root </> "log.jsonl"
    bus <- openBus root
    let writers = 4
        iters = 25
    result <-
      timeout 60_000_000 $ do
        -- Feeder: keep the persister appending so the lock is hot, and
        -- join it so quiescence below sees its last scribe.
        fed <- newEmptyMVar
        _ <- forkIO $ do
          mapM_
            ( \i -> do
                _ <- scribeIO bus (mkPost "feeder" ["hub"] ("f" <> T.pack (show i)))
                pure ()
            )
            [1 .. 40]
          putMVar fed ()
        mvars <-
          mapM
            ( \w -> do
                mv <- newEmptyMVar
                _ <- forkIO $ do
                  rs <-
                    mapM
                      ( \i ->
                          postLocal root (mkPost ("w" <> T.pack (show w)) ["hub"] ("m" <> T.pack (show (i :: Int))))
                      )
                      [1 .. iters]
                  putMVar mv rs
                pure mv
            )
            [1 .. writers]
        collected <- mapM takeMVar mvars
        takeMVar fed
        pure collected
    case result of
      Nothing -> assert "L1 lock stress: completes within 60s (pre-fix this wedges in flock)" False
      Just collected -> do
        let returned = sort (map (snd . stamp) (concat collected))
            total = writers * iters
        assert "L1 returned ids unique (postLocal never collides)" $
          length (nub returned) == total
        content <- TIO.readFile path
        let parsed = mapMaybe (unframeStored @Text) (T.lines content)
            fileIds = map (snd . stamp) parsed
        assert "L1 file ids positional and duplicate-free" $
          sort fileIds == [0 .. fromIntegral (length parsed) - 1]
        -- The persister reconciles external growth on its one-second tick;
        -- wait (bounded) until the image has pulled the externals in.
        let waitReconciled :: Int -> IO Bool
            waitReconciled 0 = pure False
            waitReconciled n = do
              imgLen <- atomically $ length . unLog <$> busImage bus
              if imgLen == length (T.lines content)
                then pure True
                else threadDelay 100_000 >> waitReconciled (n - 1)
        reconciled <- waitReconciled 100
        assert "L1 persister reconciles trailing external growth" reconciled
        rq <- atomically (awaitQuiescent bus)
        case rq of
          Left _ -> assert "L1 persister alive at quiescence" False
          Right () -> do
            img <- atomically (busImage bus)
            let imgIds = map (snd . stamp) (unLog img)
            assert "L1 image ids equal file ids" $
              imgIds == fileIds
    closeBus bus
    removePathForcibly root

  -------------------------------------------------------------------------
  -- L2: receipt stress — one sender, many rapid sequential daemon posts.
  --
  -- The client's receipt poll and the daemon's next receipt write share
  -- a path in-process; pre-fix the lazy read pinned it and the daemon's
  -- write threw "resource busy", killing the daemon thread. Pins the
  -- strict read, the mutex-serialized receipt write, and daemon survival.
  -------------------------------------------------------------------------
  putStrLn "receipt stress"

  do
    root <- tempRoot "free-agent-l2-receipts"
    tid <- forkIO (runDaemon @Text root)
    threadDelay 100_000
    let n = 100
    rs <-
      mapM
        (\i -> postViaDaemon root (mkPost "l2" ["hub"] ("m" <> T.pack (show (i :: Int)))))
        [1 .. n]
    -- Daemon alive at the end: one more post round-trips.
    fin <- postViaDaemon root (mkPost "l2" ["hub"] "final")
    killThread tid
    assert "L2 daemon survived the receipt storm" $
      snd (stamp fin) == fromIntegral n
    assert "L2 receipts contiguous in arrival order" $
      map (snd . stamp) rs == [0 .. fromIntegral n - 1]
    content <- readFileStrict (root </> "log.jsonl")
    let parsed = mapMaybe (unframeStored @Text) (T.lines content)
    assert "L2 log holds exactly the storm" $
      length parsed == n + 1
    removePathForcibly root

  -------------------------------------------------------------------------
  -- L3: dead-daemon honesty — a killed daemon makes postViaDaemon fail
  -- fast with a message naming the daemon and the root, not the 10s
  -- receipt-timeout backstop and not an opaque ENXIO.
  --
  -- The daemon's fifo handle closes via finalizer after the kill, so a
  -- full GC makes the ENXIO-at-open deterministic; the attempt stays
  -- externally bounded regardless.
  -------------------------------------------------------------------------
  putStrLn "dead-daemon honesty"

  do
    root <- tempRoot "free-agent-l3-dead-daemon"
    tid <- forkIO (runDaemon @Text root)
    threadDelay 100_000
    _ <- postViaDaemon root (mkPost "l3" ["hub"] "ping")
    killThread tid
    threadDelay 100_000
    performGC
    t0 <- getCurrentTime
    r <- timeout 5_000_000 $ try @SomeException (postViaDaemon root (mkPost "l3" ["hub"] "post"))
    dt <- diffUTCTime <$> getCurrentTime <*> pure t0
    case r of
      Nothing -> assert "L3 fails fast, not via the 10s receipt backstop" False
      Just (Right _) -> assert "L3 posting to a dead daemon must fail" False
      Just (Left e) -> do
        let msg = T.pack (displayException e)
        assert "L3 the failure names the daemon" ("daemon" `T.isInfixOf` msg)
        assert "L3 the failure names the root" (T.pack root `T.isInfixOf` msg)
        assert "L3 fails well under the 10s receipt timeout" (dt < 3)
    removePathForcibly root

  -------------------------------------------------------------------------
  -- L4: double create — fifo creation is idempotent when the path is a
  -- fifo, loud when it is anything else.
  -------------------------------------------------------------------------
  putStrLn "fifo double create"

  do
    root <- tempRoot "free-agent-l4-fifo"
    let fifo = fifoPath root
    ensureFifo fifo
    ensureFifo fifo
    st <- getFileStatus fifo
    assert "L4 double create passes on a fifo" (isNamedPipe st)
    removeFile fifo
    TIO.writeFile fifo "not a pipe"
    again <- try @SomeException (ensureFifo fifo)
    assert "L4 a regular file at bus.fifo fails loudly" $
      case again of
        Left e -> "named pipe" `T.isInfixOf` T.pack (displayException e)
        Right () -> False
    daemon <- try @SomeException (runDaemon @Text root)
    assert "L4 runDaemon refuses a non-fifo path loudly" (isLeft daemon)
    removePathForcibly root

  -------------------------------------------------------------------------
  -- L5: persister death ends seats — awaitSince answers Left instead of
  -- retrying forever, and a runSeatBus waiter halts rather than blocking
  -- on an empty durable prefix.
  -------------------------------------------------------------------------
  putStrLn "persister death"

  do
    root <- tempRoot "free-agent-l5-persister-death"
    bus <- openBus root
    seatDied <- newEmptyMVar
    _ <- forkFinally (runSeatBus bus "l5" ["l5"] (ioShard (const (pure [])))) (putMVar seatDied)
    closeBus bus
    answered <- timeout 5_000_000 (atomically (awaitSince bus ["l5"] 0))
    assert "L5 awaitSince answers with the persister's failure" $
      case answered of
        Just (Left _) -> True
        _ -> False
    died <- timeout 5_000_000 (takeMVar seatDied)
    assert "L5 a runSeatBus seat halts on a dead persister" $
      case died of
        Just (Left _) -> True
        _ -> False
    removePathForcibly root
