-- |
-- | The OpenLayers Point module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.Point (
  Point
  , RawPoint 
  
  , create
  , create' ) where

-- Standard import
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
import OpenLayers.FFI as FFI
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
foreign import createImpl::Fn2 (Array Number) (FFI.NullableOrUndefined GeometryLayout.GeometryLayout) (Effect Point)
create::Array Number->Maybe GeometryLayout.GeometryLayout->Effect Point
create c e = runFn2 createImpl c (FFI.toNullable e)

create'::Array Number->Effect Point
create' c = runFn2 createImpl c FFI.undefined
