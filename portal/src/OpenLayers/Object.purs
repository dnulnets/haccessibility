-- |
-- | The OpenLayers Control module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Object (
    module Observable
    , BaseObject
    , RawBaseObject) where

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

--
-- Foreign data types
-- 
foreign import data RawBaseObject :: Type -> Type
type BaseObject a = Observable.Observable (RawBaseObject a)
