{-# LANGUAGE OverloadedStrings #-}

-- | tensor/par probe: the stamping office as a connective toggle.
module Axioma.Tensor
  ( tensorTopic,
  )
where

import Axioma.Common (assert, mkPost)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamp, stamped, unframeStored)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Data.List (sort, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Bus (postLocal)
import Free.Agent.Bus.Daemon (postViaDaemon, runDaemon)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

tensorTopic :: IO ()
tensorTopic = do
  -------------------------------------------------------------------------
  -- Tensor/par probe (⊗/⅋): the stamping office as a connective toggle.
  --
  -- Two exchanges share a single checker. The bodies are invariant under
  -- permutation (⊗), but a stateful checker that reads in stamp order sees
  -- the interleaving (⅋). With the daemon on, one canonical order wins;
  -- with self-stamping, two different submission orders produce two
  -- different checker states.
  --
  -- Mutation guard: if postLocal ignores submission order (e.g., stamps by
  -- time), the AB/BA aggregates would be equal and the probe collapses.
  -------------------------------------------------------------------------
  putStrLn "Tensor/par probe"

  do
    tmp <- getTemporaryDirectory
    let exchangeA = [("a1", "A1"), ("a2", "A2")]
        exchangeB = [("b1", "B1"), ("b2", "B2")]
        orderedAB = exchangeA <> exchangeB
        orderedBA = exchangeB <> exchangeA
        allBodies = sort (map snd orderedAB)
        checkerOf posts =
          T.intercalate "|" $
            map (body . stamped) (sortOn (snd . stamp) posts)
        runViaDaemon :: FilePath -> [(Text, Text)] -> IO Text
        runViaDaemon root posts = do
          tid <- forkIO (runDaemon @Text root)
          threadDelay 100_000
          stamped <-
            mapM
              ( \(sender, b) ->
                  postViaDaemon root (mkPost sender ["checker"] b)
              )
              posts
          killThread tid
          pure (checkerOf stamped)
        runViaPostLocal :: FilePath -> [(Text, Text)] -> IO Text
        runViaPostLocal root posts = do
          stamped <-
            mapM
              ( \(sender, b) ->
                  postLocal root (mkPost sender ["checker"] b)
              )
              posts
          pure (checkerOf stamped)
        invariantSet :: Text -> [Stamped Text] -> Bool
        invariantSet recipient posts =
          sort (map (body . stamped) (filter (deliversToRecipient recipient) posts))
            == allBodies
          where
            deliversToRecipient r s = r `elem` to (stamped s)

    -- Office on: daemon imposes a canonical order; repeated trials agree.
    officeOnTrials <-
      mapM
        ( \n -> do
            let root = tmp </> ("free-agent-probe-on-" <> show (n :: Int))
            removePathForcibly root
            createDirectoryIfMissing True root
            ag <- runViaDaemon root orderedAB
            content <- TIO.readFile (root </> "log.jsonl")
            let parsed = mapMaybe (unframeStored @Text) (T.lines content)
            assert ("daemon-on invariant for trial " <> show n) $
              invariantSet "checker" parsed
            pure ag
        )
        [1 .. 5]
    case officeOnTrials of
      (on0 : _) -> do
        assert "daemon-on trials all agree" $
          all (== on0) (drop 1 officeOnTrials)

        -- Office off, order AB: checker sees A before B.
        let rootAB = tmp </> "free-agent-probe-off-ab"
        removePathForcibly rootAB
        createDirectoryIfMissing True rootAB
        agAB <- runViaPostLocal rootAB orderedAB
        contentAB <- TIO.readFile (rootAB </> "log.jsonl")
        let parsedAB = mapMaybe (unframeStored @Text) (T.lines contentAB)
        assert "office-off AB invariant" $ invariantSet "checker" parsedAB

        -- Office off, order BA: checker sees B before A.
        let rootBA = tmp </> "free-agent-probe-off-ba"
        removePathForcibly rootBA
        createDirectoryIfMissing True rootBA
        agBA <- runViaPostLocal rootBA orderedBA
        contentBA <- TIO.readFile (rootBA </> "log.jsonl")
        let parsedBA = mapMaybe (unframeStored @Text) (T.lines contentBA)
        assert "office-off BA invariant" $ invariantSet "checker" parsedBA

        -- The witness: shared checker is order-sensitive.
        assert "office-off AB differs from BA (shared checker is order-sensitive)" $
          agAB /= agBA

        -- The daemon's canonical order matches the AB submission order used above.
        assert "daemon-on aggregate matches office-off AB" $
          on0 == agAB

        mapM_
          removePathForcibly
          [ tmp </> ("free-agent-probe-on-" <> show n)
          | n <- [1 .. 5] :: [Int]
          ]
        removePathForcibly rootAB
        removePathForcibly rootBA
      _ -> assert "daemon-on produced at least one trial" False
