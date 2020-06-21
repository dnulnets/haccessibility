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
  , create'
  
  ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn0
  , Fn1
  , runFn0
  , runFn1)

-- Effect imports
import Effect (Effect)

-- Openlayers
import OpenLayers.FFI as FFI
import OpenLayers.View as View
import OpenLayers.Layer.Base as Base
import OpenLayers.PluggableMap (
  PluggableMap
  , addInteraction
  , addLayer
  , getView
  , setTarget
  , clearTarget) as PluggableMap
--
-- Foreign data types
-- 
foreign import data RawMap :: Type
type Map = PluggableMap.PluggableMap RawMap
--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Map)
create :: forall r . Maybe {|r} -> Effect Map
create o = runFn1 createImpl $ FFI.toNullable o

create':: Effect Map
create' = runFn1 createImpl FFI.undefined
