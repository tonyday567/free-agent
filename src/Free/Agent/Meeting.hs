{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A pure meeting driver and the replay oracle (stage 5 of endgame-path).
--
-- A meeting is rounds over a roster: round 1 sees the seed, each later
-- round sees the previous round's outputs.  The log is the seed followed
-- by the concatenated rounds — oldest first, exactly the shape 'cone' and
-- 'branches' resolve against.
--
-- Replay is re-running the same meeting with one box swapped (different
-- model, fixed prompt).  The oracles pin:
--
--   * a fresh identical roster reproduces the log exactly;
--   * posts whose cone avoids the swapped name reproduce identically
--     ('unchanged');
--   * posts downstream of the swap differ;
--   * thread edges are now exact 'PostId's assigned positionally by the
--     driver, so a same-named swap is visible as soon as it reaches a
--     thread edge.
--
-- Approach taken: 'AgentBox' now receives both the input posts and their
-- positional ids for the current round.  The driver assigns ids to the
-- concatenated outputs after each round, so every post in the returned
-- log carries the correct ids and every reply/synthesis can cite them.
module Free.Agent.Meeting
  ( AgentBox (..),
    quoter,
    runAgentBox,
    withIds,
    meet,
    meetLog,
    unchanged,
  )
where

import Circuit.Agent (Agent, Name, Post (..), PostId, coneByIndex)
import Circuit.Agent.Query (synthesisPosts)
import Circuit.Poly (System, runSystem, system, monoDir, monoIn)
import Circuit.ChannelPoly qualified as CP
import Data.List (inits, intersect)
import Data.Text (Text)
import Data.Text qualified as T

-- | Existential box around a pure batch agent, carrying its current state.
--
-- The agent input is a pair: the batch of posts and the positional ids the
-- driver assigned to that batch.  This lets agents thread replies/syntheses
-- by exact reference without inventing ids.
data AgentBox where
  AgentBox :: s -> Agent (->) s ([Post Text], [PostId]) [Post Text] -> AgentBox

-- | One batch step: absorb the input and its ids, emit the answer
-- ('iterateSystem' semantics — output is read from the post-input state).
runAgentBox :: AgentBox -> [Post Text] -> [PostId] -> ([Post Text], AgentBox)
runAgentBox (AgentBox s ag) ins ids =
  let s' = snd (CP.runSystem ag s) (ins, ids)
      (outs, _) = CP.runSystem ag s'
   in (outs, AgentBox s' ag)

-- | Lift an agent that ignores ids into one that accepts the boxed
-- @(posts, ids)@ input.  This is the minimal adapter for existing agents
-- that do not need provenance.
withIds :: Agent (->) s [Post Text] [Post Text] -> Agent (->) s ([Post Text], [PostId]) [Post Text]
withIds ag = system $ \(s, d) ->
  let (ins, _ids) = monoDir d
   in runSystem ag (s, monoIn ins)

-- | A deterministic oracle agent: answers each batch with one honest
-- synthesis post ('synthesisPosts') whose body quotes what it saw.  @tag@
-- marks the "model" — swapping the box is swapping the tag.
--
-- Output depends only on the pre-input state (the Moore idiom of 'tape'):
-- the state carries the last batch and its ids seen, and the emit answers
-- it.
quoter :: Name -> Text -> Agent (->) ([Post Text], [PostId], [Post Text], [PostId]) ([Post Text], [PostId]) [Post Text]
quoter who tag = system $ \((hist, histIds, batch, batchIds), d) ->
  let (ins, insIds) = monoDir d
      out = case batch of
        [] -> []
        _ -> synthesisPosts who batch batchIds (tag <> " saw " <> T.pack (show (map from batch)))
   in ((reverse ins ++ hist, reverse insIds ++ histIds, ins, insIds), (out, ()))

-- | Run @n@ rounds over a roster.  Every box sees the previous round's
-- concatenated outputs (round 1 sees the seed).  Returns the rounds.
meet :: Int -> [AgentBox] -> [Post Text] -> [[Post Text]]
meet n boxes seed = map (map snd) rounds
  where
    (rounds, _) = meetWithIds n boxes (zip [0 ..] seed) (fromIntegral (length seed))

-- | Internal driver that threads positional ids through the rounds.
meetWithIds :: Int -> [AgentBox] -> [(PostId, Post Text)] -> PostId -> ([[(PostId, Post Text)]], [AgentBox])
meetWithIds 0 boxes _ _ = ([], boxes)
meetWithIds k boxes ins nextId =
  let (outs, boxes') = runRound boxes (map snd ins) (map fst ins)
      n = length outs
      outIds = take n [nextId ..]
      stamped = zip outIds outs
      (rest, boxes'') = meetWithIds (k - 1) boxes' stamped (nextId + fromIntegral n)
   in (stamped : rest, boxes'')

-- | One round: every box sees the same batch; outputs concatenate in
-- roster order.
runRound :: [AgentBox] -> [Post Text] -> [PostId] -> ([Post Text], [AgentBox])
runRound boxes ins ids =
  let results = map (\b -> runAgentBox b ins ids) boxes
   in (concatMap fst results, map snd results)

-- | The full log of a meeting: seed followed by the rounds, oldest first.
-- Ids are assigned positionally by the driver, so 'thread' fields in the
-- returned posts are valid ids against this log ('indexToIdMap' reproduces
-- the same assignment).
meetLog :: Int -> [AgentBox] -> [Post Text] -> [Post Text]
meetLog n boxes seed = seed ++ concat (meet n boxes seed)

-- | The posts of a log whose ancestry cone avoids the given names — the
-- subtrees a swap of those names cannot touch.
unchanged :: [Name] -> [Post Text] -> [Post Text]
unchanged banned posts =
  [ p
  | (prior, p) <- zip (inits posts) posts,
    null (coneByIndex prior p `intersect` banned)
  ]
