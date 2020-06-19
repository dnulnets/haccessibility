-- |
-- | The OpenLayers Control module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Object (
    module Observable
    , module Event
    
    , BaseObject
    , RawBaseObject

    , ObjectEvent
    , RawObjectEvent) where

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

-- Our own imports
import OpenLayers.Observable (Observable) as Observable
import OpenLayers.Events.Event(BaseEvent) as Event

--
-- Foreign data types
-- 
foreign import data RawBaseObject :: Type -> Type
type BaseObject a = Observable.Observable (RawBaseObject a)

foreign import data RawObjectEvent :: Type -> Type
type ObjectEvent a = Event.BaseEvent a
