-- | Conversation tape readers for meeting transcripts.
module Free.Agent.Diagram
  ( meetingSkeleton,
    skeletonLabels,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Poly.StringDiagram (SDiagram (..))
import Data.List (delete, elemIndex, mapAccumL)
import Data.Text qualified as T

-- | The drawing skeleton of a conversation: each post is a box labelled by
-- its sender, wired by its 'thread' ancestry — "the log is the diagram of
-- the meeting that produced it".  A root is @SBox label 0 1@ (nothing
-- feeds it), a reply @SBox label 1 1@, and a synthesis with m parents is
-- preceded by a visible merge spider ('SSpider' @m 1@); a post cited as
-- parent by k > 1 later posts forks its output through a visible copy
-- spider ('SSpider' @1 k@).
--
-- One pass over the log, oldest first: the state is the list of live wire
-- ends (each tagged by the thread edge it feeds, or by the post whose
-- uncited output it carries to the boundary), and each post emits one
-- layer — permute the parent wires to the back (adjacent 'SSwap's), merge,
-- box, fork.  A dangling thread edge (parent not in the log) becomes a
-- free input wire, present from the left boundary.
meetingSkeleton :: [Post a] -> SDiagram
meetingSkeleton [] = SWire
meetingSkeleton ps = foldr1 SThenD (snd (mapAccumL step dangling [0 .. length ps - 1]))
  where
    -- thread edges are exact positional ids into the oldest-first log;
    -- 'Nothing' is a dangling id (expected not to happen for a well-formed
    -- stamped meeting).
    sources =
      [ [if i < fromIntegral j then Just i else Nothing | i <- thread p]
      | (j, p) <- zip ([0 ..] :: [Int]) ps
      ]
    -- the citation edges of post j, in consumer (log) order
    cited j = [(i, pos) | (i, ss) <- zip [0 ..] sources, (pos, Just j') <- zip [0 ..] ss, j' == fromIntegral j]
    -- one free input wire per dangling edge
    dangling = [Right (i, pos) | (i, ss) <- zip [0 ..] sources, (pos, Nothing) <- zip [0 ..] ss]

    step :: [Either Int (Int, Int)] -> Int -> ([Either Int (Int, Int)], SDiagram)
    step live i = (rest ++ outs, layer)
      where
        label = T.unpack (from (ps !! i))
        m = length (thread (ps !! i))
        nW = length live
        -- bring the m parent wires to the back, in thread order, filling
        -- positions right to left so each bubble only crosses unfixed wires
        (swaps, ordered) =
          foldl' bubble ([], live) (zip [nW - 1, nW - 2 ..] (reverse (map (edge i) [0 .. m - 1])))
        bubble (ss, xs) (t, w) = case elemIndex w xs of
          Nothing -> error "meetingSkeleton: parent wire not live"
          Just p -> (ss ++ [p .. t - 1], take t (delete w xs) ++ [w] ++ drop t (delete w xs))
        rest = take (nW - m) ordered
        outs = case cited i of
          [] -> [Left i]
          es -> map (uncurry edge) es
        mergeBox = case m of
          0 -> SBox label 0 1
          1 -> SBox label 1 1
          _ -> SThenD (SSpider m 1) (SBox label 1 1)
        consumedPart
          | length outs > 1 = SThenD mergeBox (SSpider 1 (length outs))
          | otherwise = mergeBox
        body = case rest of
          [] -> consumedPart
          rs -> SBeside (wires (length rs)) consumedPart
        layer = case swaps of
          [] -> body
          ss -> SThenD (foldl1 SThenD (map (swapAt nW) ss)) body

    edge i pos = Right (i, pos)

    -- adjacent swap at index s on a bundle of n wires
    swapAt n s =
      foldr1
        SBeside
        ([wires s | s > 0] ++ [SSwap] ++ [wires (n - s - 2) | n - s - 2 > 0])

    -- identity on a bundle of r >= 1 wires
    wires 1 = SWire
    wires r = SBeside SWire (wires (r - 1))

-- | The box labels of a skeleton, in composition order.
skeletonLabels :: SDiagram -> [String]
skeletonLabels SWire = []
skeletonLabels (SBox l _ _) = [l]
skeletonLabels (SSpider _ _) = ["spider"]
skeletonLabels SPrismBox = ["prism"]
skeletonLabels (SBeside f g) = skeletonLabels f ++ skeletonLabels g
skeletonLabels (SThenD f g) = skeletonLabels f ++ skeletonLabels g
skeletonLabels SBend = ["cup"]
skeletonLabels SBend' = ["cap"]
skeletonLabels (STurn f) = skeletonLabels f
skeletonLabels SUnitL = ["unitL"]
skeletonLabels SUnitL' = ["unitL'"]
skeletonLabels SUnitR = ["unitR"]
skeletonLabels SUnitR' = ["unitR'"]
skeletonLabels SAssoc = ["assoc"]
skeletonLabels SAssoc' = ["assoc'"]
skeletonLabels SSwap = ["swap"]
skeletonLabels (STrace f) = skeletonLabels f
