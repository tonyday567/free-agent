{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | File-log lock and same-process handle-exclusion witnesses.
module Axioma.Lifecycle
  ( lifecycleTopic,
  )
where

import Axioma.Common (assert, mkPost, tempRoot)
import Circuit.Agent.Framing (Log (..), Stamped, stamp)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, displayException, try)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Bus (postLocal, readLogStrict, withBusFiles)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode), withFile)
import System.Timeout (timeout)

lifecycleTopic :: IO ()
lifecycleTopic = do
  -------------------------------------------------------------------------
  -- A bus-path handle held in this process excludes another bus-path writer.
  -- The contender must wait for handle release rather than interleaving an
  -- open/write window with the holder.
  -------------------------------------------------------------------------
  putStrLn "same-process bus-handle exclusion"
  do
    root <- tempRoot "free-agent-handle-exclusion"
    _ <- postLocal root (mkPost "seed" ["seat"] "seed")
    started <- newEmptyMVar
    result <- newEmptyMVar
    early <- withBusFiles $ withFile (root </> "log.jsonl") AppendMode $ \_ -> do
      _ <- forkIO $ do
        putMVar started ()
        r <- try @SomeException (postLocal root (mkPost "writer" ["seat"] "held"))
        putMVar result r
      takeMVar started
      threadDelay 50_000
      timeout 1_000_000 (takeMVar result)
    assert "a same-process bus writer waits for an open log handle" $
      case early of
        Nothing -> True
        Just _ -> False
    settled <- timeout 5_000_000 (takeMVar result)
    case settled of
      Just (Left err) -> assert "contending writer failed" False >> putStrLn (displayException err)
      _ -> pure ()
    assert "the contender completes after handle release" $
      case settled of
        Just (Right _) -> True
        _ -> False
    Log (posts :: [Stamped Text]) <- readLogStrict (root </> "log.jsonl")
    assert "both posts survive the excluded handle windows" $
      length posts == 2
    removePathForcibly root

  -------------------------------------------------------------------------
  -- Concurrent locked appends assign unique file-positional ids and lose no
  -- posts. This tests the file kernel itself, without a persister or image.
  -------------------------------------------------------------------------
  putStrLn "locked concurrent file appends"
  do
    root <- tempRoot "free-agent-lock-stress"
    let writers = 4
        iterations = 25
        total = writers * iterations
    result <- timeout 60_000_000 $ do
      dones <- sequence [newEmptyMVar | _ <- [1 .. writers]]
      mapM_
        ( \(w, done) ->
            forkIO $ do
              ids <-
                mapM
                  ( \i ->
                      snd . stamp
                        <$> postLocal root (mkPost ("w" <> T.pack (show w)) ["seat"] (T.pack (show (w, i))))
                  )
                  [1 .. iterations]
              putMVar done ids
        )
        (zip [1 :: Int ..] dones)
      mapM takeMVar dones
    case result of
      Nothing -> assert "lock stress completes within 60s" False
      Just collected -> do
        assert "locked appends return every unique file id exactly once" $
          sort (concat collected) == [0 .. total - 1]
        Log (posts :: [Stamped Text]) <- readLogStrict (root </> "log.jsonl")
        assert "the file contains every locked append exactly once" $
          sort (map (snd . stamp) posts) == [0 .. total - 1]
    removePathForcibly root
