-- |
-- | The OpenLayers module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.Polygon ( Polygon, RawPolygon ) where

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

import OpenLayers.Geom.SimpleGeometry as SimpleGeometry

--
-- Foreign data types
-- 
foreign import data RawPolygon :: Type

-- |The Polygon type with an inheritance from Geometry
type Polygon = SimpleGeometry.SimpleGeometry RawPolygon

--
-- Function mapping
--
