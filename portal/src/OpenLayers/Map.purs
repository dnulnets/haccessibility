-- |
-- | The OpenLayers Map module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Map (
  Map

  , create
  
  , addLayer

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

--
-- Foreign data types
-- 
foreign import data Map :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Map))

create :: forall r . {|r} -> Effect (Maybe Map)
create o = toMaybe <$> runFn1 createImpl o

--
-- add functions
--
foreign import addLayerImpl :: forall l . Fn2 (Base.Base l) Map (Effect Unit)

addLayer :: forall l . Base.Base l -> Map -> Effect Unit
addLayer o = runFn2 addLayerImpl o

--
-- get functions
--
foreign import getViewImpl :: Fn1 Map (Effect (Nullable View.View))

getView :: Map -> Effect (Maybe View.View)
getView o = toMaybe <$> runFn1 getViewImpl o
