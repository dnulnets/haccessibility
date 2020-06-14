-- |
-- | The OpenLayers SimpleGeometry module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.SimpleGeometry ( SimpleGeometry, RawSimpleGeometry ) where

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
