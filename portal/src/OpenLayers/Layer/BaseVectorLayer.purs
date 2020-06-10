-- |
-- | The OpenLayers Geometry module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.BaseVectorLayer ( BaseVectorLayer, RawBaseVectorLayer ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , Fn3
  , Fn4
  , Fn5
  , runFn1
  , runFn2
  , runFn3
  , runFn4
  , runFn5)

-- Effect imports
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

-- Our own imports
import OpenLayers.Layer.Base as Base

--
-- Foreign data types
-- 
foreign import data RawBaseVectorLayer :: Type -> Type

-- |The actual abstract basetype for geometry
type BaseVectorLayer a = Base.Base (RawBaseVectorLayer a)

--
-- Function mapping
--
