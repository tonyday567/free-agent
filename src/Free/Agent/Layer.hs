{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Layer instance for 'FreeAgent': unit / run / bind into any category.
--
-- Mirrors 'Circuit.Free' so free agent terms obey the same β/η laws as the
-- circuits free-category layer.
module Free.Agent.Layer
  ( runFreeAgent,
    bindFreeAgent,
  )
where

import Circuit.Category (Category (..))
import Circuit.Layer (Layer (..), (:~>))
import Data.Kind (Type)
import Free.Agent.Syntax (FreeAgent (..))
import Prelude hiding (id, (.))

-- $setup
-- >>> import Free.Agent.Syntax
-- >>> import Free.Agent.Layer
-- >>> import Prelude hiding (id, (.))

-- | 'FreeAgent' is a free category, so it is a 'Layer' over any base arrow.
-- 'runFreeAgent' and 'bindFreeAgent' are the two folds.
instance Layer FreeAgent where
  type Law FreeAgent arr' = Category arr'
  type Run FreeAgent arr = Category arr
  type Bind FreeAgent arr = ()
  unit = Lift
  bind ::
    forall arr' arr a b.
    (Law FreeAgent arr') =>
    (arr :~> arr') ->
    FreeAgent arr a b ->
    arr' a b
  bind h (Lift f) = h f
  bind h (Compose @_ @b1 g f) = bind h g . bind h f

-- | Fold a free agent term back into its base category.
runFreeAgent ::
  forall (arr :: Type -> Type -> Type) a b.
  (Category arr) =>
  FreeAgent arr a b ->
  arr a b
runFreeAgent (Lift f) = f
runFreeAgent (Compose g f) = runFreeAgent g . runFreeAgent f

-- | Fold a free agent term into any target category.
bindFreeAgent ::
  forall (arr' :: Type -> Type -> Type) (arr :: Type -> Type -> Type) a b.
  (Category arr') =>
  (arr :~> arr') ->
  FreeAgent arr a b ->
  arr' a b
bindFreeAgent = bind
