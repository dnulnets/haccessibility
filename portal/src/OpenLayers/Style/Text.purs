-- |
-- | The OpenLayers Text module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Style.Text (
  Text
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

-- own imports
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data Text :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Text)

create :: forall r . Maybe {|r} -> Effect Text
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect Text
create' = runFn1 createImpl FFI.undefined
