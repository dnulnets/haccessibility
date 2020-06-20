-- |
-- | The OpenLayers Control module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Control (
    defaults
    , defaults') where

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

-- Own imports
import OpenLayers.Control.Control as Control
import OpenLayers.Collection as Collection
import OpenLayers.FFI as FFI
--
-- Function mapping
--
foreign import defaultsImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect (Collection.Collection Control.Control))
defaults :: forall r . Maybe {|r} -> Effect (Collection.Collection Control.Control)
defaults o = runFn1 defaultsImpl (FFI.toNullable o)

defaults' :: Effect (Collection.Collection Control.Control)
defaults' = runFn1 defaultsImpl FFI.undefined
