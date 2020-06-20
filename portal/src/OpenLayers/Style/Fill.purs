-- |
-- | The OpenLayers Fill module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Style.Fill (
  Fill
  , create
  , create'
  ) where

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
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data Fill :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Fill)

create :: forall r . Maybe {|r} -> Effect Fill
create r = runFn1 createImpl (FFI.toNullable r)

create' :: Effect Fill
create' = runFn1 createImpl FFI.undefined
