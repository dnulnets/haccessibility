-- |
-- | The OpenLayers Base module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.Base ( Base, RawBase ) where

--
-- Foreign data types
-- 
foreign import data RawBase :: Type -> Type

-- |The actual abstract basetype for geometry
type Base a = RawBase a

--
-- Function mapping
--
