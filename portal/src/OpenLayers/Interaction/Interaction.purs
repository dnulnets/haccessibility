-- |
-- | The OpenLayers Intersecto Select module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Interaction.Interaction
  ( module Object
    , Interaction
    , RawInteraction) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (
  Fn1
  , Fn3
  , runFn1
  , runFn3)

-- Effect imports
import Effect (Effect)

-- Import our own stuff
import OpenLayers.Object (BaseObject) as Object

--
-- Foreign data types
-- 
foreign import data RawInteraction :: Type->Type
type Interaction a = Object.BaseObject (RawInteraction a)
