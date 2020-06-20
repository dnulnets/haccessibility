-- |
-- | The OpenLayers Circle module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Style.Circle (
  Circle
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
foreign import data Circle :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Circle)

create :: forall r . Maybe {|r} -> Effect Circle
create r = runFn1 createImpl (FFI.toNullable r)

create' :: Effect Circle
create' = runFn1 createImpl FFI.undefined
