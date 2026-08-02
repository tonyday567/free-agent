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
--   * a /same-named/ swap is structurally invisible — 'thread' names are
--     identical between the two logs.  Names are labels, not identity;
--     this is the recorded limit that the PostId gate (stage 5 👤)
--     watches.  The gate stays shut while replay re-contains the shared
--     prefix by value; it opens the first time two conversations must
--     cite the same node /by reference/.
module Free.Agent.Meeting
  ( AgentBox (..),
    quoter,
    runAgentBox,
    meet,
    meetLog,
    unchanged,
  )
where

import Circuit.Agent (Agent, Name, Post (..), cone)
import Circuit.Agent.Cli (synthesisPosts)
import Circuit.Poly (System (..), monoDir)
import Circuit.Poly.Process (runSystem)
import Data.List (inits, intersect)
import Data.Text (Text)
import Data.Text qualified as T

-- | Existential box around a pure batch agent, carrying its current state.
data AgentBox where
  AgentBox :: s -> Agent (->) s [Post Text] [Post Text] -> AgentBox

-- | One batch step: absorb the input, emit the answer ('iterateSystem'
-- semantics — output is read from the post-input state).
runAgentBox :: AgentBox -> [Post Text] -> ([Post Text], AgentBox)
runAgentBox (AgentBox s ag) ins =
  let s' = snd (runSystem ag s) ins
      (outs, _) = runSystem ag s'
   in (outs, AgentBox s' ag)

-- | A deterministic oracle agent: answers each batch with one honest
-- synthesis post ('synthesisPosts') whose body quotes what it saw.  @tag@
-- marks the "model" — swapping the box is swapping the tag.
--
-- Output depends only on the pre-input state (the Moore idiom of 'tape'):
-- the state carries the last batch seen, and the emit answers it.
quoter :: Name -> Text -> Agent (->) ([Post Text], [Post Text]) [Post Text] [Post Text]
quoter who tag = System $ \((hist, batch), d) ->
  let ins = monoDir d
      out = case batch of
        [] -> []
        _ -> synthesisPosts who batch (tag <> " saw " <> T.pack (show (map from batch)))
   in ((reverse ins ++ hist, ins), (out, ()))

-- | Run @n@ rounds over a roster.  Every box sees the previous round's
-- concatenated outputs (round 1 sees the seed).  Returns the rounds.
meet :: Int -> [AgentBox] -> [Post Text] -> [[Post Text]]
meet 0 _ _ = []
meet n boxes ins =
  let (outs, boxes') = runRound boxes ins
   in outs : meet (n - 1) boxes' outs

-- | One round: every box sees the same batch; outputs concatenate in
-- roster order.
runRound :: [AgentBox] -> [Post Text] -> ([Post Text], [AgentBox])
runRound boxes ins =
  let results = map (`runAgentBox` ins) boxes
   in (concatMap fst results, map snd results)

-- | The full log of a meeting: seed followed by the rounds, oldest first.
meetLog :: Int -> [AgentBox] -> [Post Text] -> [Post Text]
meetLog n boxes seed = seed ++ concat (meet n boxes seed)

-- | The posts of a log whose ancestry cone avoids the given names — the
-- subtrees a swap of those names cannot touch.
unchanged :: [Name] -> [Post Text] -> [Post Text]
unchanged banned posts =
  [ p
  | (prior, p) <- zip (inits posts) posts,
    null (cone prior p `intersect` banned)
  ]
