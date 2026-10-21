{-# LANGUAGE OverloadedStrings #-}

-- | FreeSeat bundle primitives + tensor laws: folds to shard citizens,
-- fork/plain agreement.
module Axioma.Bundle
  ( bundleTopic,
  )
where

import Axioma.Common (assert, mkPost)
import Circuit.Agent (AgentSeat (..), Post (..), cellToAgent, raceA, runAgentShard, tape)
import Circuit.Agent.Tensor
  ( awaitShard,
    closeShardIO,
    fanInShard,
    fanOutShard,
    raceShard,
    silentShard,
  )
import Data.Text (Text)
import Data.Text qualified as T
import Free.Agent.Pipeline (Pipeline, mapP, pipelineShard, routeP)
import Free.Agent.Seat
  ( awaitSeat,
    bundleSeat,
    fanInSeat,
    fanOutSeat,
    forkSeat,
    interpretSeat,
    interpretSeatA,
    pipelineSeat,
    raceSeat,
    silentSeat,
  )

bundleTopic :: IO ()
bundleTopic = do
  -------------------------------------------------------------------------
  -- Bundle primitives (FreeSeat fold / agreement)
  --
  -- The shard-level tensor laws live in circuits-agent-axioma.  Here we only
  -- witness that the free syntax folds to those citizens and agrees with the
  -- agent-level semantics where applicable.
  -------------------------------------------------------------------------
  putStrLn "Bundle primitives"

  putStrLn "silent seat folds to silentShard"
  do
    let posts = [mkPost "human" ["bot"] "x"]
    (outsFree, _) <- closeShardIO (interpretSeat silentSeat) posts []
    (outsShard, _) <- closeShardIO silentShard posts []
    assert "silent seat folds to silentShard" $ outsFree == outsShard

  putStrLn "fork is id"
  do
    let p :: Pipeline (Post Text) (Post Text)
        p = mapP (\x -> x {body = "m:" <> body x})
        posts = [mkPost "human" ["bot"] "hi"]
    (outs1, _) <- closeShardIO (interpretSeat (pipelineSeat p)) posts []
    (outs2, _) <- closeShardIO (interpretSeat (forkSeat (pipelineSeat p))) posts []
    assert "fork seat agrees with plain seat" $ outs1 == outs2

  putStrLn "awaitSeat folds to awaitShard"
  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        posts = [mkPost "human" [] "one"]
    (outsFree, _) <- closeShardIO (interpretSeat (awaitSeat a b)) posts []
    (outsShard, _) <-
      closeShardIO
        (awaitShard (pipelineShard (routeP (const [p]))) (pipelineShard (routeP (const [q]))))
        posts
        []
    assert "awaitSeat folds to awaitShard" $ outsFree == outsShard

  putStrLn "raceSeat folds to raceShard and agrees with raceA"
  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        posts = [mkPost "human" [] "one"]
        (outsPure, _) =
          runAgentShard
            (raceA (cellToAgent (tape (const [p]))) (cellToAgent (tape (const [q]))))
            (AgentSeat ([], []) [])
            posts
    (outsFree, _) <- closeShardIO (interpretSeat (raceSeat a b)) posts []
    (outsShard, _) <-
      closeShardIO
        (raceShard (pipelineShard (routeP (const [p]))) (pipelineShard (routeP (const [q]))))
        posts
        []
    assert "raceSeat folds to raceShard" $ outsFree == outsShard
    assert "raceSeat agrees with raceA" $ outsFree == outsPure

  putStrLn "fanOutSeat folds to fanOutShard"
  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        posts = [mkPost "human" [] "one"]
    (outsFree, _) <- closeShardIO (interpretSeat (fanOutSeat [a, b])) posts []
    (outsShard, _) <-
      closeShardIO
        (fanOutShard [pipelineShard (routeP (const [p])), pipelineShard (routeP (const [q]))])
        posts
        []
    assert "fanOutSeat folds to fanOutShard" $ outsFree == outsShard

  putStrLn "fanInSeat folds to fanInShard"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "sum" [] (T.intercalate "+" (map body (concat pss)))]
        posts = [mkPost "human" [] "one"]
    (outsFree, _) <- closeShardIO (interpretSeat (fanInSeat summary [a, b])) posts []
    (outsShard, _) <-
      closeShardIO
        ( fanInShard
            summary
            [ pipelineShard (routeP (\x -> [x {body = body x <> "-a"}])),
              pipelineShard (routeP (\x -> [x {body = body x <> "-b"}]))
            ]
        )
        posts
        []
    assert "fanInSeat folds to fanInShard" $ outsFree == outsShard

  putStrLn "bundle is fan-in over forks"
  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "bundle" [] (T.intercalate "|" (map body (concat pss)))]
        posts = [mkPost "human" [] "one"]
    (outs, _) <- closeShardIO (interpretSeat (bundleSeat summary [forkSeat a, forkSeat b])) posts []
    assert "bundle: summary is one post" $ length outs == 1
    assert "bundle: summary body collects fork outputs" $
      case outs of
        [o] -> body o == "one-a|one-b"
        _ -> False

  -------------------------------------------------------------------------
  -- FreeSeat tensor laws (syntax-level) and agent-level agreement
  -------------------------------------------------------------------------
  putStrLn "FreeSeat tensor laws"

  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        r = mkPost "test" [] "r"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        c = pipelineSeat (routeP (const [r]))
        posts = [mkPost "human" [] "one"]
        leftA = awaitSeat a (awaitSeat b c)
        rightA = awaitSeat (awaitSeat a b) c
    (outsL, _) <- closeShardIO (interpretSeat leftA) posts []
    (outsR, _) <- closeShardIO (interpretSeat rightA) posts []
    assert "SeatAwait associative under interpretSeat" $ outsL == outsR
    assert "SeatAwait interpretSeatA agrees with interpretSeat" $
      interpretSeatA leftA posts == outsL

  do
    let p = mkPost "test" [] "p"
        q = mkPost "test" [] "q"
        r = mkPost "test" [] "r"
        a = pipelineSeat (routeP (const [p]))
        b = pipelineSeat (routeP (const [q]))
        c = pipelineSeat (routeP (const [r]))
        posts = [mkPost "human" [] "one"]
        leftR = raceSeat a (raceSeat b c)
        rightR = raceSeat (raceSeat a b) c
    (outsL, _) <- closeShardIO (interpretSeat leftR) posts []
    (outsR, _) <- closeShardIO (interpretSeat rightR) posts []
    assert "SeatRace associative under interpretSeat" $ outsL == outsR
    assert "SeatRace interpretSeatA agrees with interpretSeat" $
      interpretSeatA leftR posts == outsL

  do
    let p = mkPost "test" [] "p"
        a = pipelineSeat (routeP (const [p]))
        posts = [mkPost "human" [] "one"]
    (outsA, _) <- closeShardIO (interpretSeat a) posts []
    (outsAwaitRight, _) <- closeShardIO (interpretSeat (awaitSeat a silentSeat)) posts []
    (outsAwaitLeft, _) <- closeShardIO (interpretSeat (awaitSeat silentSeat a)) posts []
    (outsRaceRight, _) <- closeShardIO (interpretSeat (raceSeat a silentSeat)) posts []
    (outsRaceLeft, _) <- closeShardIO (interpretSeat (raceSeat silentSeat a)) posts []
    assert "silentSeat is right unit for SeatAwait" $ outsAwaitRight == outsA
    assert "silentSeat is left unit for SeatAwait" $ outsAwaitLeft == outsA
    assert "silentSeat is right unit for SeatRace (left bias)" $ outsRaceRight == outsA
    assert "silentSeat is left unit for SeatRace (right bias)" $ outsRaceLeft == outsA

  do
    let a = pipelineSeat (routeP (\x -> [x {body = body x <> "-a"}]))
        b = pipelineSeat (routeP (\x -> [x {body = body x <> "-b"}]))
        summary pss = [mkPost "sum" [] (T.intercalate "+" (map body (concat pss)))]
        fan = fanInSeat summary [a, b]
        posts = [mkPost "human" [] "one"]
    (outsShard, _) <- closeShardIO (interpretSeat fan) posts []
    assert "SeatFanIn interpretSeatA agrees with interpretSeat" $
      interpretSeatA fan posts == outsShard
