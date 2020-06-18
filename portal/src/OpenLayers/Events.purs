-- |
-- | The OpenLayers Control module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Events (
    EventsKey
    , ListenerFunction) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , runFn1)

-- Effect imports
import Effect (Effect)

-- Import our own stuff
import OpenLayers.Events.Event (BaseEvent) as Event

--
-- Foreign data types
-- 
foreign import data EventsKey :: Type

type ListenerFunction a = Event.BaseEvent a->Effect Boolean
