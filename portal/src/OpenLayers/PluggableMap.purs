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
