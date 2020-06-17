-- |
-- | The OpenLayers Control module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Control (defaults) where

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

-- Own imports
import OpenLayers.Control.Control as Control
import OpenLayers.Collection as Collection

--
-- Function mapping
--
foreign import defaultsImpl :: forall r . Fn1 {|r} (Effect (Collection.Collection Control.Control))

defaults :: forall r . {|r} -> Effect (Collection.Collection Control.Control)
defaults o = runFn1 defaultsImpl o
