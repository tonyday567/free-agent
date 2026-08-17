{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Free category over a base arrow, named for the agent seat.
--
-- Same shape as 'Circuit.Free.Free': 'Lift' embeds a base arrow, 'Compose'
-- sequences free morphisms. Package role ABOVE circuits-agent — reify seats
-- and pipelines here, then fold into 'Circuit.Agent.Agent' or 'Circuit.Agent.Shard'.
module Free.Agent.Syntax
  ( FreeAgent (..),
  )
where

import Circuit.Category (Category (..), Discrete (..))
import Prelude hiding (id, (.))

-- | Free category over a base arrow @arr@.
--
-- * 'Lift' — embed a base arrow.
-- * 'Compose' — sequential composition (right-to-left as in 'Category').
--
-- Intermediate 'Ob' evidence lives on 'Compose' so same-category folds do not
-- require a 'Discrete' base for every intermediate object.
data FreeAgent arr a b where
  -- | Embed a base arrow as a single generator.
  Lift :: arr a b -> FreeAgent arr a b
  -- | Sequence two free morphisms (right-to-left, matching 'Category' composition).
  Compose :: (Ob arr b) => FreeAgent arr b c -> FreeAgent arr a b -> FreeAgent arr a c

instance (Category arr) => Category (FreeAgent arr) where
  type Ob (FreeAgent arr) a = Ob arr a
  id = Lift id
  (.) = Compose

-- | A discrete base category lifts to a discrete free category.
--
-- 'withOb' delegates to the base category, so any object constraint can be
-- discharged at every free object. This is what lets 'bindFreeAgent' fold
-- into arbitrary discrete target categories.
instance (Discrete arr) => Discrete (FreeAgent arr) where
  withOb @a x = withOb @arr @a x
