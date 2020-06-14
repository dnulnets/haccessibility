-- |
-- | The OpenLayers Geometry module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geom.Geometry ( Geometry, RawGeometry ) where

--
-- Foreign data types
-- 
foreign import data RawGeometry :: Type -> Type

-- |The actual abstract basetype for geometry
type Geometry a = RawGeometry a

--
-- Function mapping
--
