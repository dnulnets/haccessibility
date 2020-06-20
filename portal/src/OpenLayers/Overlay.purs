-- |
-- | The OpenLayers Overlay module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Overlay (
  Overlay
  , create
  , create') where

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
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data Overlay :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Overlay)

create :: forall r . Maybe {|r} -> Effect Overlay
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect Overlay
create' = runFn1 createImpl FFI.undefined
