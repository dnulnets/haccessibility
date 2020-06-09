-- |
-- | The OpenLayers SimpleGeometry module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.SimpleGeometry ( SimpleGeometry, RawSimpleGeometry ) where

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
import OpenLayers.Geom.Geometry as Geometry

--
-- Foreign data types
-- 
foreign import data RawSimpleGeometry :: Type -> Type

-- |The actual abstract basetype for geometry
type SimpleGeometry a = Geometry.Geometry (RawSimpleGeometry a)

--
-- Function mapping
--
