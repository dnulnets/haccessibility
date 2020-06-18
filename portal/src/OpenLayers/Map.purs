-- |
-- | The OpenLayers Map module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Map (
  module PluggableMap
  , Map
  , RawMap

  , create
  
  ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , runFn1
  , runFn2)

-- Effect imports
import Effect (Effect)

-- Openlayers
import OpenLayers.View as View
import OpenLayers.Layer.Base as Base
import OpenLayers.PluggableMap (PluggableMap, addInteraction, addLayer, getView) as PluggableMap
--
-- Foreign data types
-- 
foreign import data RawMap :: Type
type Map = PluggableMap.PluggableMap RawMap
--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Map))

create :: forall r . {|r} -> Effect (Maybe Map)
create o = toMaybe <$> runFn1 createImpl o
