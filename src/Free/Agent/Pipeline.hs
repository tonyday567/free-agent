{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
    routeTo,
    routeBy,
    broadcast,
    forName,
    fromName,
  )
where

import Circuit (Body (..))
import Circuit.Agent (Name, Post (..), deliversTo)
import Circuit.Category (Category (..), K (..))
import Circuit.Poles (Poles (..), poles0)
import Data.Text (Text)
import Prelude hiding (id, (.))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Free.Agent.Pipeline
-- >>> import Circuit.Agent

-- | A pipeline stage or composition of stages.
data Pipeline a b where
  -- | Keep only inputs that satisfy the predicate.
  Filter :: (a -> Bool) -> Pipeline a a
  -- | Transform each input.
  Map :: (a -> b) -> Pipeline a b
  -- | Expand each input into zero or more outputs.
  Route :: (a -> [b]) -> Pipeline a b
  -- | Sequence two pipelines.
  Compose :: Pipeline b c -> Pipeline a b -> Pipeline a c

-- | Pipelines form a category: 'id' is @'Map' id@; composition is 'Compose'.
instance Category Pipeline where
  id = Map id
  (.) = Compose

-- | Keep only inputs that satisfy the predicate.
filterP :: (a -> Bool) -> Pipeline a a
filterP = Filter

-- | Transform each input.
mapP :: (a -> b) -> Pipeline a b
mapP = Map

-- | Expand each input into zero or more outputs.
routeP :: (a -> [b]) -> Pipeline a b
routeP = Route

-- | Route every post to a single recipient.
routeTo :: Name -> Pipeline (Post Text) (Post Text)
routeTo name = Map (\p -> p {to = [name]})

-- | Route posts using a function from the post to a recipient list.
routeBy :: (Post Text -> [Name]) -> Pipeline (Post Text) (Post Text)
routeBy f = Map (\p -> p {to = f p})

-- | Broadcast every post to a list of recipients.
broadcast :: [Name] -> Pipeline (Post Text) (Post Text)
broadcast names = Map (\p -> p {to = names})

-- | Keep posts addressed to @name@ (via 'deliversTo').
forName :: Name -> Pipeline (Post Text) (Post Text)
forName name = Filter (`deliversTo` [name])

-- | Keep posts whose sender is @name@.
fromName :: Name -> Pipeline (Post Text) (Post Text)
fromName name = Filter (\p -> from p == name)

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
pipelineShard :: Pipeline a b -> Poles (Body (,) [a] (K IO)) [a] [b]
pipelineShard p = poles0 writeBatch readBatch
  where
    writeBatch = Body $ K $ \(_, xs) -> pure (xs, ())
    readBatch = Body $ K $ \(s, ()) -> pure ([], runPipeline p s)
