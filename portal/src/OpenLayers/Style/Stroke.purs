-- |
-- | The OpenLayers Stroke module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Style.Stroke (
  Stroke
  , create
  , create') where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn1, runFn1)

-- Effect imports
import Effect (Effect)

-- Own imports
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data Stroke :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Stroke)

create :: forall r . Maybe {|r} -> Effect Stroke
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect Stroke
create' = runFn1 createImpl FFI.undefined
