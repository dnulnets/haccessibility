-- |
-- | The OpenLayers Openstreetmap API module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Source.OSM (
  OSM
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

-- Import own stuff
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data OSM :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect OSM)

create :: forall r . Maybe {|r} -> Effect OSM
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect OSM
create' = runFn1 createImpl FFI.undefined
