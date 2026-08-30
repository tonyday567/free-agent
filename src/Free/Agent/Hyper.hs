-- | The hypergraph generators for posts and agents (stage 3 of
-- endgame-path).
--
-- A hypergraph category lets wires fork and merge.  At the stream level
-- the generators are 'copyP' (fork: duplicate a stream) and 'mergeP'
-- (merge: append two streams); 'braidP' swaps the two middle blocks —
-- the permutation whose block-versus-wire confusion was the bug this
-- library family grew out of.  With append as merge the bialgebra law
-- holds for streams of posts /exactly/:
--
-- @
-- copyP (mergeP (xs, ys)) == merge2 (braidP (copy2 (xs, ys)))
--   -- both sides are (xs ++ ys, xs ++ ys)
-- @
--
-- At the agent level, 'both' is the merge of two agents — the join of
-- the extension semilattice, dual to sequencing: both agents see the
-- /same/ input (copy), their outputs are appended (merge).  This is the
-- combinator form of graph 'overlay' / a two-seat roster.
--
-- = Semantics on record
--
-- Merge is /bag/ semantics: 'both a a' double-posts.  Idempotence is
-- name-keyed, not structural — seat registries dedupe by name; streams
-- do not dedupe at all.  Bag at the wire, set at the name.
module Free.Agent.Hyper
  ( copyP,
    copy2,
    mergeP,
    merge2,
    braidP,
    silent,
    both,
  )
where

import Circuit.Moore (Moore (..), moore, mooreMorphism)
import Circuit.Poly (Mono)

-- | Fork: duplicate a stream.
copyP :: [a] -> ([a], [a])
copyP xs = (xs, xs)

-- | Fork two streams at once (block-level).
copy2 :: ([a], [b]) -> (([a], [a]), ([b], [b]))
copy2 (xs, ys) = (copyP xs, copyP ys)

-- | Merge: append two streams.  Bag semantics — no deduplication.
mergeP :: ([a], [a]) -> [a]
mergeP (xs, ys) = xs ++ ys

-- | Merge two stream-pairs componentwise: each inner pair is appended.
-- This is @mergeP ⊗ mergeP@, the merge half of the bialgebra.
merge2 :: (([a], [a]), ([a], [a])) -> ([a], [a])
merge2 ((xs, ys), (zs, ws)) = (mergeP (xs, ys), mergeP (zs, ws))

-- | Swap the two middle blocks: @((a,b),(c,d)) → ((a,c),(b,d))@.
braidP :: ((a, b), (c, d)) -> ((a, c), (b, d))
braidP ((a, b), (c, d)) = ((a, c), (b, d))

-- | The silent agent: emits nothing.  Additive zero for 'both'.
silent :: Moore (,) (->) () (Mono a [b])
silent = moore (\((), _) -> ((), ([], ())))

-- | Merge two bundle-output agents: both see the same input, outputs are
-- appended in left-then-right order.  The state is the pair.
--
-- Laws (oracle-pinned): commutative up to output bag; 'silent' is a zero
-- on either side; /not/ idempotent — @both a a@ double-posts (bag at the
-- wire).
both ::
  Moore (,) (->) s1 (Mono a [b]) ->
  Moore (,) (->) s2 (Mono a [b]) ->
  Moore (,) (->) (s1, s2) (Mono a [b])
both x y = moore $ \((s1, s2), d) ->
  let (s1', (o1, ())) = mooreMorphism x (s1, d)
      (s2', (o2, ())) = mooreMorphism y (s2, d)
   in ((s1', s2'), (o1 ++ o2, ()))
