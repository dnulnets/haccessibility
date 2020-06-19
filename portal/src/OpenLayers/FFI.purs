-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.FFI (
    NullableOrUndefined
    , null
    , undefined
    , toNullableOrUndefined
    , toMaybe
  ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe(..), maybe)
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , Fn3
  , runFn1
  , runFn2
  , runFn3)

-- Effect imports
import Effect (Effect)

--
-- Our own imports
--
import OpenLayers.Geom.Geometry as Geometry
import OpenLayers.Style.Style as Style

--
-- Foreign data types
-- 
foreign import data NullableOrUndefined :: Type -> Type

-- | The null or undefined value.
foreign import null :: forall a. NullableOrUndefined a
foreign import undefined :: forall a. NullableOrUndefined a

foreign import nullableOrUndefined :: forall a r. Fn3 (NullableOrUndefined a) r (a -> r) r

-- | Wrap a non-null value.
foreign import notNullOrUndefined :: forall a. a -> NullableOrUndefined a

-- | Takes `Nothing` to `null`, and `Just a` to `a`.
toNullableOrUndefined :: forall a. Maybe a -> NullableOrUndefined a
toNullableOrUndefined = maybe null notNullOrUndefined

toMaybe :: forall a. NullableOrUndefined a -> Maybe a
toMaybe n = runFn3 nullableOrUndefined n Nothing Just
