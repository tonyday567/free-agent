{-# LANGUAGE OverloadedStrings #-}

-- | lifecycle hygiene (wave H): the log lock, the stamping daemon, and
-- death honesty. L1-L5.
module Axioma.Lifecycle
  ( lifecycleTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (stamp, stamped, unLog, unframeStored)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM (atomically)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (awaitQuiescent, busImage, closeBus, openBus, postLocal, scribeIO)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
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
  -- the red evidence is an externally-killed binary.
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
