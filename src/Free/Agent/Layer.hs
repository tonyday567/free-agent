{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}

-- | Layer instance for 'FreeAgent': unit / run / bind into any discrete target.
--
-- Mirrors 'Circuit.Free' so free agent terms obey the same β/η laws as the
-- circuits free-category layer.
module Free.Agent.Layer
  ( runFreeAgent,
    bindFreeAgent,
  )
where

import Circuit.Category (Category (..), Discrete (..), ObDict (..), withObDict)
import Circuit.Layer (Layer (..), (:~>))
import Data.Kind (Type)
import Free.Agent.Syntax (FreeAgent (..))
import Prelude hiding (id, (.))

-- $setup
-- >>> import Free.Agent.Syntax
-- >>> import Free.Agent.Layer
-- >>> import Prelude hiding (id, (.))

instance Layer FreeAgent where
  type Law FreeAgent arr' = Discrete arr'
  type Run FreeAgent arr = (Category arr, Discrete arr)
  type Bind FreeAgent arr = ()
  unit = Lift
  bind ::
    forall arr' arr a b.
    (Law FreeAgent arr', Bind FreeAgent arr, Ob arr a, Ob arr b, Ob arr' a, Ob arr' b) =>
    (forall s. ObDict arr s -> ObDict arr' s) ->
    (arr :~> arr') ->
    FreeAgent arr a b ->
    arr' a b
  bind _phi h (Lift f) = h f
  bind phi h (Compose @_ @b1 g f) =
    withObDict (phi (ObDict :: ObDict arr b1)) $
      bind phi h g . bind phi h f

-- | Fold a free agent term back into its base category.
runFreeAgent ::
  forall (arr :: Type -> Type -> Type) a b.
  (Category arr, Ob arr a, Ob arr b) =>
  FreeAgent arr a b ->
  arr a b
runFreeAgent (Lift f) = f
runFreeAgent (Compose g f) = runFreeAgent g . runFreeAgent f

-- | Fold a free agent term into any discrete target category.
bindFreeAgent ::
  forall (arr' :: Type -> Type -> Type) (arr :: Type -> Type -> Type) a b.
  (Category arr', Discrete arr', Ob arr a, Ob arr b, Ob arr' a, Ob arr' b) =>
  (forall s. ObDict arr s -> ObDict arr' s) ->
  (arr :~> arr') ->
  FreeAgent arr a b ->
  arr' a b
bindFreeAgent _phi h (Lift f) = h f
bindFreeAgent phi h (Compose @_ @b1 g f) =
  withObDict (phi (ObDict :: ObDict arr b1)) $
    bindFreeAgent phi h g . bindFreeAgent phi h f
