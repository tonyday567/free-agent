-- | Derivations: the 2-cells where the two dimensions meet (stage 6 of
-- endgame-path).
--
-- A derivation is a square: the horizontal edge is the agent box (the
-- sender — process dimension); the vertical edges are the ancestry,
-- resolved to strictly-earlier posts (record dimension).  With honest
-- ancestry ('synthesisPosts', stage 4) the square is /recoverable from
-- the log alone/ — the record projects the derivation, which is the
-- unification claim of the endgame.
--
-- Pasting:
--
--   * /horizontal/ — merging two boxes ('both') pastes their squares side
--     by side: the merged box derives the same log as the two boxes in a
--     roster.  Oracle: @meetLog n [both a b] seed == meetLog n [a, b] seed@.
--
--   * /vertical/ — stacking squares in time: chasing resolved parents
--     from any post walks the pasting back to the roots.  Oracle: the
--     chase from the last post of a meeting covers the whole log.
module Free.Agent.Derivation
  ( Derivation (..),
    derivation,
    valid,
    chaseLog,
  )
where

import Circuit.Agent (Post (..))
import Data.List (find)

-- | A derivation square: @dPost@ is the output (the square's target);
-- @dParents@ are the resolved vertical sources, in 'thread' order.
-- Dangling edges resolve to nothing, so @dParents@ may be shorter than
-- @thread dPost@ — 'valid' checks exactly this.
data Derivation a = Derivation
  { dPost :: Post a,
    dParents :: [Post a]
  }
  deriving (Show, Eq)

-- | The square of a post against the posts prior to it (oldest first).
-- Each thread edge resolves to the most recent prior post by that name —
-- the same resolution as 'branches'.
derivation :: (Eq a) => [Post a] -> Post a -> Derivation a
derivation prior p =
  Derivation
    p
    [ q
    | n <- thread p,
      Just q <- [find ((== n) . from) (reverse prior)]
    ]

-- | A square is valid when every vertical edge resolves to a
-- strictly-earlier post — no dangling ancestry.
valid :: (Eq a) => [Post a] -> Post a -> Bool
valid prior p = length (dParents (derivation prior p)) == length (thread p)

-- | The posts reachable from the last post by chasing derivations back
-- through the log (vertical pasting made executable).  When ancestry is
-- honest and the log is one meeting, the chase covers the whole log.
--
-- Posts are compared by value; duplicate posts collapse (bag at the wire,
-- but the chase is a set walk).
chaseLog :: (Eq a) => [Post a] -> [Post a]
chaseLog [] = []
chaseLog posts = go [] [last posts]
  where
    priorOf p = takeWhile (/= p) posts
    go acc [] = acc
    go acc (p : ps)
      | p `elem` acc = go acc ps
      | otherwise = go (p : acc) (dParents (derivation (priorOf p) p) ++ ps)
