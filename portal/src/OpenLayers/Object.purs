-- |
-- | The OpenLayers Object module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Object (
    module Observable
    , module Event

    , BaseObject
    , RawBaseObject    
    , get

    , ObjectEvent
    , RawObjectEvent) where

-- Standard import
import Prelude
import Foreign

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried
  ( Fn1
  , Fn2

  , runFn1
  , runFn2)

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

--
-- Functions
--

-- TODO: This is a really ugly version of a get, no type safety :-(
-- Really, really, really need to fix this!!!!
foreign import getImpl :: forall a . Fn2 String (BaseObject a) (Effect Foreign)
get :: forall o v . String -> BaseObject o -> Effect (Maybe v)
get n self = do
    f <- runFn2 getImpl n self
    case isNull f || isUndefined f of
        true ->
            pure Nothing
        false ->
            pure $ Just $ unsafeFromForeign f
