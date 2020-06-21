-- |
-- | The OpenLayers Polygon module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.Polygon (
  Polygon

  , create
  , create'

  , RawPolygon ) where

import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (
  Fn2
  , runFn2)

-- Effect imports
import Effect (Effect)

-- Our own imports
import OpenLayers.Geom.SimpleGeometry as SimpleGeometry
import OpenLayers.Geom.GeometryLayout as GeometryLayout
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data RawPolygon :: Type

-- |The Polygon type with an inheritance from Geometry
type Polygon = SimpleGeometry.SimpleGeometry RawPolygon

--
-- Function mapping
--
foreign import createImpl::Fn2 (Array (Array Number)) (FFI.NullableOrUndefined GeometryLayout.GeometryLayout) (Effect Polygon)
create::Array (Array Number)->Maybe GeometryLayout.GeometryLayout->Effect Polygon
create ap l = runFn2 createImpl ap (FFI.toNullable l)

create'::Array (Array Number)->Effect Polygon
create' ap = runFn2 createImpl ap FFI.undefined
