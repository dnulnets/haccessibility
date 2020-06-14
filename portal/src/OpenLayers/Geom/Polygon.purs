-- |
-- | The OpenLayers Polygon module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.Polygon (
  Polygon
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

--
-- Foreign data types
-- 
foreign import data RawPolygon :: Type

-- |The Polygon type with an inheritance from Geometry
type Polygon = SimpleGeometry.SimpleGeometry RawPolygon

--
-- Function mapping
--
foreign import createImpl::Fn2 (Array (Array Number)) (Nullable GeometryLayout.GeometryLayout) (Effect (Nullable Polygon))
create::Array (Array Number)->Maybe GeometryLayout.GeometryLayout->Effect (Maybe Polygon)
create ap l = toMaybe <$> runFn2 createImpl ap (toNullable l)