-- |
-- | The OpenLayers module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.Point (
  Point
  , RawPoint 
  
  , create ) where

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
import OpenLayers.Geom.SimpleGeometry as SimpleGeometry
import OpenLayers.Geom.GeometryLayout as GeometryLayout

--
-- Foreign data types
-- 
foreign import data RawPoint :: Type

-- |The Polygon type with an inheritance from Geometry
type Point = SimpleGeometry.SimpleGeometry RawPoint

--
-- Function mapping
--
foreign import createImpl::Fn2 (Array Number) (Nullable GeometryLayout.GeometryLayout) (Effect (Nullable Point))
create::Array Number->Maybe GeometryLayout.GeometryLayout->Effect (Maybe Point)
create c e = toMaybe <$> runFn2 createImpl c (toNullable e)
