{-# LANGUAGE OverloadedStrings #-}

-- | STM seat interpretation: interpretSeatS agrees with pure and shard
-- behaviour.
module Axioma.Stm
  ( stmTopic,
  )
where

import Axioma.Common (assert, mkPost)
import Circuit.Agent (Post (..), toBag)
import Circuit.Agent.Tensor (closeShardIO)
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Pipeline (Pipeline, filterP, mapP, routeP, runPipeline)
import Free.Agent.Pipeline qualified as P
import Free.Agent.Seat
  ( FreeSeat (..),
    awaitSeat,
    fanInSeat,
    interpretSeat,
    interpretSeatA,
    interpretSeatS,
    pipelineSeat,
    raceSeat,
    runAgentSBox,
    silentSeat,
  )

stmTopic :: IO ()
stmTopic = do
  -------------------------------------------------------------------------
  -- STM seat interpretation (interpretSeatS)
  -------------------------------------------------------------------------
  putStrLn "STM seat interpretation"

  putStrLn "STM pipeline agrees with pure behaviour"
  do
    let p = mapP (\x -> x {body = "s:" <> body x}) `P.Compose` filterP (\x -> body x /= "noise")
        posts =
          [ mkPost "human" ["bot"] "hello",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "world"
          ]
    outsS <- runAgentSBox (interpretSeatS (pipelineSeat p)) posts
    let outsA = interpretSeatA (pipelineSeat p) posts
    assert "interpretSeatS pipeline equals interpretSeatA" $ outsS == outsA
    assert "interpretSeatS pipeline equals runPipeline" $ outsS == runPipeline p posts

  putStrLn "STM silent seat emits nothing"
  do
    let posts = [mkPost "human" ["bot"] "x"]
    outsS <- runAgentSBox (interpretSeatS silentSeat) posts
    assert "silent seat STM output is empty" $ null outsS

  putStrLn "STM composition agrees with pure behaviour"
  do
    let p1 = filterP (\x -> body x /= "noise") :: Pipeline (Post Text) (Post Text)
        p2 = mapP (\x -> x {body = "m:" <> body x}) :: Pipeline (Post Text) (Post Text)
        seat = SeatCompose (pipelineSeat p2) (pipelineSeat p1)
        posts =
          [ mkPost "human" ["bot"] "hello",
            mkPost "human" ["bot"] "noise",
            mkPost "human" ["bot"] "world"
          ]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    assert "interpretSeatS composition equals interpretSeatA" $ outsS == interpretSeatA seat posts

  putStrLn "STM await agrees with pure behaviour and shard fold"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        seat = awaitSeat a b
        posts = [mkPost "human" [] "one"]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    (outsShard, _) <- closeShardIO (interpretSeat seat) posts []
    assert "interpretSeatS await equals interpretSeatA" $ outsS == interpretSeatA seat posts
    assert "interpretSeatS await equals interpretSeat shard" $ outsS == outsShard

  putStrLn "STM race is left-biased like pure behaviour"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        seat = raceSeat a b
        posts = [mkPost "human" [] "one"]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    assert "interpretSeatS race equals interpretSeatA" $ outsS == interpretSeatA seat posts

  putStrLn "STM fan-out/fan-in agrees with pure behaviour"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "sum" [] (T.intercalate "+" (map body (concat pss)))]
        seat = fanInSeat summary [a, b]
        posts = [mkPost "human" [] "one", mkPost "human" [] "two"]
    outsS <- runAgentSBox (interpretSeatS seat) posts
    assert "interpretSeatS fanIn equals interpretSeatA" $ outsS == interpretSeatA seat posts

  putStrLn "STM bag invariant under input permutation"
  do
    let p = mapP (\x -> x {body = "p:" <> body x}) :: Pipeline (Post Text) (Post Text)
        posts =
          [ mkPost "human" ["bot"] "one",
            mkPost "human" ["bot"] "two",
            mkPost "human" ["bot"] "three"
          ]
        perm = reverse posts
    outs1 <- runAgentSBox (interpretSeatS (pipelineSeat p)) posts
    outs2 <- runAgentSBox (interpretSeatS (pipelineSeat p)) perm
    assert "bag of outputs is invariant under input permutation" $
      toBag outs1 == toBag outs2
