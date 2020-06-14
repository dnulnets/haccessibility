-- |
-- | The OpenLayers BaseTileLayers module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.BaseTileLayer ( BaseTileLayer, RawBaseTileLayer ) where

-- Own imports
import OpenLayers.Layer.Base as Base

--
-- Foreign data types
-- 
foreign import data RawBaseTileLayer :: Type -> Type

-- |The actual abstract basetype for geometry
type BaseTileLayer a = Base.Base (RawBaseTileLayer a)

--
-- Function mapping
--
