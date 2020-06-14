-- |
-- | The OpenLayers BaseVectorLayer module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.BaseVectorLayer (
  BaseVectorLayer
  , RawBaseVectorLayer ) where

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
