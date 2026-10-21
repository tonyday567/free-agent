{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for the free-agent axioma topic modules.
module Axioma.Common
  ( assert,
    iterateAgent,
    runMono,
    closeBusDurable,
    mkPost,
    tempRoot,
    boxLabels,
    countSpiders,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Diagram (SDiagram (..))
import Circuit.Machine (Body (Body), Moore, MooreT (..), scan)
import Circuit.Tensor (unConst)
import Control.Concurrent.STM (atomically)
import Data.Either (isRight)
import Data.Text (Text)
import Free.Agent.Bus (Bus, awaitQuiescent, closeBus)
import Free.Agent.Diagram (skeletonLabels)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, openTempFile)

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

-- | The list runner: 'scan' over a seeded 'MooreT' (post-step
-- convention — output read from the state after each input).
iterateAgent :: Moore (,) s (->) i o -> s -> [i] -> [o]
iterateAgent (Body _ k o) s0 = scan (MooreT (Body (\i -> k (s0, i)) k o))

-- | The Eval-shaped reading of a body state: emit, and the step
-- function as an input-indexed state update.
runMono :: Moore (,) s (->) i o -> s -> (o, i -> s)
runMono (Body _ k o) s = (unConst (o s), \i -> k (s, i))

-- | Close a bus through the watermark idiom: wait (by predicate, never
-- handoff) until the image is on disk, assert the persister lived, stop
-- the thread. Under the ack-less scribe this replaces the old
-- "scribeIO blocked, so the file is complete by now" assumption.
closeBusDurable :: Bus Text -> IO ()
closeBusDurable bus = do
  r <- atomically (awaitQuiescent bus)
  closeBus bus
  assert "persister alive at close" $ isRight r

mkPost :: Text -> [Text] -> Text -> Post Text
mkPost a ds = Post a ds []

-- | Per-run unique directory under the temp root: @TMPDIR/name-XXXXXX@,
-- created fresh through 'openTempFile''s exclusive naming. Sibling suite
-- instances used to share fixed names and orphan each other's daemons on
-- unlinked fifos (wave H); a unique root per run kills that class. The
-- directory exists on return; callers should 'removePathForcibly' when
-- done.
tempRoot :: String -> IO FilePath
tempRoot name = do
  tmp <- getTemporaryDirectory
  (p, h) <- openTempFile tmp (name <> "-")
  hClose h
  removeFile p
  createDirectoryIfMissing True p
  pure p

-- | The 'SBox' labels of a meeting skeleton, in composition order (meeting
-- skeletons contain only boxes, spiders and swaps besides wires).
boxLabels :: SDiagram -> [String]
boxLabels = filter (`notElem` ["spider", "swap"]) . skeletonLabels

-- | Count the spiders of a given arity in a diagram tree.
countSpiders :: (Int, Int) -> SDiagram -> Int
countSpiders (m, n) (SSpider m' n') = if m == m' && n == n' then 1 else 0
countSpiders mn (SBeside f g) = countSpiders mn f + countSpiders mn g
countSpiders mn (SThenD f g) = countSpiders mn f + countSpiders mn g
countSpiders _ _ = 0
