-- |
-- | The OpenLayers PluggableMap module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.PluggableMap (
  PluggableMap
  , RawPluggableMap

  , addLayer
  , addInteraction

  , getView

  , setTarget
  , clearTarget

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
import OpenLayers.FFI as FFI
import OpenLayers.View as View
import OpenLayers.Layer.Base as Base
import OpenLayers.Interaction.Interaction as Interaction

--
-- Foreign data types
-- 
foreign import data RawPluggableMap :: Type->Type
type PluggableMap a = RawPluggableMap a
--
-- Function mapping
--

--
-- add functions
--
foreign import addLayerImpl :: forall l m . Fn2 (Base.Base l) (PluggableMap m) (Effect Unit)

addLayer :: forall l m . Base.Base l -> PluggableMap m -> Effect Unit
addLayer o = runFn2 addLayerImpl o

foreign import addInteractionImpl :: forall i m . Fn2 (Interaction.Interaction i) (PluggableMap m) (Effect Unit)

addInteraction :: forall i m . Interaction.Interaction i -> PluggableMap m -> Effect Unit
addInteraction i = runFn2 addInteractionImpl i

--
-- get functions
--
foreign import getViewImpl :: forall m . Fn1 (PluggableMap m) (Effect (Nullable View.View))

getView :: forall m . PluggableMap m -> Effect (Maybe View.View)
getView o = toMaybe <$> runFn1 getViewImpl o

--
-- set function
--

foreign import setTargetImpl :: forall m . Fn2 (FFI.NullableOrUndefined String) (PluggableMap m) (Effect Unit)

setTarget::forall m . Maybe String -> PluggableMap m -> Effect Unit
setTarget s self = runFn2 setTargetImpl (FFI.toNullable s) self

clearTarget::forall m . PluggableMap m -> Effect Unit
clearTarget self = runFn2 setTargetImpl FFI.undefined self
