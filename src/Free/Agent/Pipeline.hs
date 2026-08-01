{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Inspectable shard pipelines over addressed posts.
--
-- A 'Pipeline' is a reified sequence of pure list-transformer stages.  Each
-- stage consumes a stream of @a@ and produces a stream of @b@; the whole
-- pipeline folds into a single pure function and is then wrapped as a stateful
-- 'Circuit.Agent.Shard'.
module Free.Agent.Pipeline
  ( Pipeline (..),
    runPipeline,
    pipelineShard,
    filterP,
    mapP,
    routeP,
  )
where

import Circuit (Ends (..), endsK)
import Circuit.Agent (Shard)
import Control.Monad.State (State, get, put)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Free.Agent.Pipeline
-- >>> import Circuit.Agent

-- | A pipeline stage or composition of stages.
data Pipeline a b where
  Filter :: (a -> Bool) -> Pipeline a a
  Map :: (a -> b) -> Pipeline a b
  Route :: (a -> [b]) -> Pipeline a b
  Compose :: Pipeline b c -> Pipeline a b -> Pipeline a c

-- | Keep only inputs that satisfy the predicate.
filterP :: (a -> Bool) -> Pipeline a a
filterP = Filter

-- | Transform each input.
mapP :: (a -> b) -> Pipeline a b
mapP = Map

-- | Expand each input into zero or more outputs.
routeP :: (a -> [b]) -> Pipeline a b
routeP = Route

-- | Fold a pipeline into a pure list function.
runPipeline :: Pipeline a b -> ([a] -> [b])
runPipeline (Filter p) = filter p
runPipeline (Map f) = map f
runPipeline (Route f) = concatMap f
runPipeline (Compose g f) = runPipeline g . runPipeline f

-- | Run a pipeline as a closed stateful shard.
--
-- The state holds the pending input batch.  Commit replaces it; emit applies
-- the pipeline and clears the buffer.
pipelineShard ::
  forall a b.
  Pipeline a b ->
  Shard (State [a]) [a] [b]
pipelineShard p =
  endsK
    (\xs -> put xs)
    ( do
        xs <- get
        put []
        pure (runPipeline p xs)
    )
