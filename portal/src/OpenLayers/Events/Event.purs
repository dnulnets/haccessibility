-- |
-- | The OpenLayers Event module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Events.Event (
    BaseEvent
    , RawBaseEvent) where

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

--
-- Foreign data types
-- 
foreign import data RawBaseEvent :: Type -> Type
type BaseEvent a = RawBaseEvent a
