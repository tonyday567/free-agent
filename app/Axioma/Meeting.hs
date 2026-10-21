{-# LANGUAGE OverloadedStrings #-}

-- | meeting skeletons, panel meeting, replay, derivations as 2-cells.
module Axioma.Meeting
  ( meetingTopic,
  )
where

import Axioma.Common (assert, boxLabels, countSpiders, mkPost)
import Circuit.Agent (Post (..), branchesByIndex, coneByIndex, replyTo, sortNub, synthesis)
import Circuit.Diagram (SDiagram (..))
import Circuit.Diagram.Hyper
  ( BoundaryEnd (..),
    HyperGraph (..),
    PortDir (..),
    PortEnd (..),
    Wire (..),
    hyperEquiv,
    normalise,
  )
import Data.List (inits)
import Data.Text qualified as T
import Free.Agent.Diagram (meetingSkeleton, skeletonLabels)
import Free.Agent.Lineage (chaseLog, dParents, lineage, valid)
import Free.Agent.Meeting (AgentBox (..), meetLog, quoter, unchanged)

meetingTopic :: IO ()
meetingTopic = do
  -------------------------------------------------------------------------
  -- Meeting skeleton (endgame stage 2/2b): a conversation reads back as a
  -- drawing of the full post-DAG — chains, forks (visible copy spiders)
  -- and syntheses (visible merge spiders), routed with SSwap permutations.
  -------------------------------------------------------------------------
  putStrLn "meeting skeleton"
  do
    let seed = mkPost "human" ["a"] "start"
        a1 = replyTo "a" 0 seed "ack"
        b1 = replyTo "b" 1 a1 "ack"
        a2 = replyTo "a" 2 b1 "ack"
        convo = [seed, a1, b1, a2]
    assert "skeleton of a ping-pong is the speaker chain (hyper-normal form)" $
      hyperEquiv (meetingSkeleton convo) $
        SThenD (SBox "human" 0 1) (SThenD (SBox "a" 1 1) (SThenD (SBox "b" 1 1) (SBox "a" 1 1)))
    assert "single-thread ancestry is the skeleton's label sequence" $
      case branchesByIndex [seed, a1, b1] a2 of
        [path] -> skeletonLabels (meetingSkeleton convo) == reverse (map T.unpack path)
        _ -> False

  putStrLn "meeting skeleton: forks, syntheses, dangling parents"
  do
    let p0 = mkPost "a" [] "seed"
        r1 = replyTo "b" 0 p0 "r1"
        r2 = replyTo "c" 0 p0 "r2"
        forkSkel = meetingSkeleton [p0, r1, r2]
        hgFork = normalise forkSkel
    assert "fork: one wire joins the parent's output to both replies' inputs" $
      Wire [] [PortEnd "a" Out 0, PortEnd "b" In 0, PortEnd "c" In 0] `elem` hgWires hgFork
    assert "fork: no free inputs, both replies reach the boundary" $
      hgInArity hgFork == 0 && hgOutArity hgFork == 2
    assert "fork: the copy spider is visible in the drawing" $
      "spider" `elem` skeletonLabels forkSkel

  do
    let pA = mkPost "a" [] "1"
        pB = mkPost "b" [] "2"
        pC = mkPost "c" [] "3"
        s = synthesis "s" [] [0, 1, 2] "sum"
        synSkel = meetingSkeleton [pA, pB, pC, s]
        hgSyn = normalise synSkel
    assert "synthesis: the box input descends from a merge of exactly the three parents' outputs" $
      Wire [] [PortEnd "a" Out 0, PortEnd "b" Out 0, PortEnd "c" Out 0, PortEnd "s" In 0] `elem` hgWires hgSyn
    assert "synthesis: no free inputs, one boundary output" $
      hgInArity hgSyn == 0 && hgOutArity hgSyn == 1
    assert "synthesis: the merge spider is visible in the drawing" $
      "spider" `elem` skeletonLabels synSkel

  do
    let d = (mkPost "a" [] "x") {thread = [99]}
        hgD = normalise (meetingSkeleton [d])
    assert "dangling parent: a free input wire feeds the box" $
      hgInArity hgD == 1 && Wire [InB 0] [PortEnd "a" In 0] `elem` hgWires hgD

  -------------------------------------------------------------------------
  -- Panel meeting (stage L2d): the shape of the committed SVG artifact —
  -- one seed fork, crossing round-2 wires, one synthesis merge.
  -------------------------------------------------------------------------
  putStrLn "panel meeting"
  do
    let seed = mkPost "human" ["agent-1", "agent-2", "agent-3"] "Q"
        a1 = replyTo "agent-1" 0 seed "A1"
        b1 = replyTo "agent-2" 0 seed "B1"
        c1 = replyTo "agent-3" 0 seed "C1"
        a2 = replyTo "agent-1" 2 b1 "A2"
        b2 = replyTo "agent-2" 3 c1 "B2"
        c2 = replyTo "agent-3" 1 a1 "C2"
        synth = synthesis "synth" ["human"] [4, 5, 6] "S"
        skel = meetingSkeleton [seed, a1, b1, c1, a2, b2, c2, synth]
    assert "panel skeleton has the 8 boxes in label order" $
      boxLabels skel
        == ["human", "agent-1", "agent-2", "agent-3", "agent-1", "agent-2", "agent-3", "synth"]
    assert "the seed's fork spider is visible (SSpider 1 3)" $
      countSpiders (1, 3) skel == 1
    assert "the synthesis merge spider is visible (SSpider 3 1)" $
      countSpiders (3, 1) skel == 1

  -- Replay (endgame stage 5): swap one box, re-derive the meeting.
  -- Deterministic quoters stand in for models; the tag is the "model".
  putStrLn "replay"
  do
    let box who tg = AgentBox ([], [], [], []) (quoter who tg)
        roster tg = [box "a" "A", box "b" tg, box "c" "C"]
        seed = [mkPost "human" ["panel"] "Q"]
        log1 = meetLog 2 (roster "B") seed
        log2 = meetLog 2 (roster "B-ALT") seed
    assert "replay with a fresh identical roster reproduces the log" $
      log1 == meetLog 2 (roster "B") seed
    assert "the swap takes effect in round 1" $
      log1 !! 2 /= log2 !! 2
    assert "unchanged subtrees reproduce identical posts" $
      unchanged ["b"] log1 == unchanged ["b"] log2
    assert "unchanged subtree is seed plus the unswapped round-1 posts" $
      length (unchanged ["b"] log1) == 3
    assert "posts downstream of the swap differ" $
      drop 4 log1 /= drop 4 log2
    assert "thread ids reproduce exactly between identical replays" $
      map thread log1 == map thread log2

  -- Lineage as 2-cells (endgame stage 6): the squares are recoverable
  -- from the log; pasting is roster merge (horizontal) and time (vertical).
  putStrLn "derivations as 2-cells"
  do
    let box who tg = AgentBox ([], [], [], []) (quoter who tg)
        seed = [mkPost "human" ["panel"] "Q"]
        log3 = meetLog 2 [box "a" "A", box "b" "B", box "c" "C"] seed
    assert "every square in a meeting log is valid" $
      and [valid prior p | (prior, p) <- zip (inits log3) log3]
    assert "vertical pasting: the chase from the last post covers its cone" $
      sortNub (map from (chaseLog log3)) == coneByIndex (init log3) (last log3)
    assert "vertical pasting: the chase visits exactly the cone names" $
      sortNub (map from (chaseLog (take 4 log3))) == coneByIndex (take 3 log3) (log3 !! 3)
    assert "a square's vertical sources are the resolved parents" $
      case drop 5 log3 of
        (p : _) ->
          let prior = take 5 log3
           in map from (dParents (lineage prior p))
                == map (\i -> from (prior !! fromIntegral i)) (thread p)
        _ -> False
