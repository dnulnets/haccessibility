-- |
-- | The OpenLayers Collection module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Collection (
    Collection
    , RawCollection

    , create
    , create'

    , extend) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , runFn1
  , runFn2)

-- Effect imports
import Effect (Effect)

-- Own imports
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data RawCollection :: Type->Type
type Collection a = RawCollection a

--
-- Function mapping
--
foreign import createImpl :: forall r t . Fn1 (FFI.NullableOrUndefined {|r}) (Effect (Collection t))

create :: forall r t . Maybe {|r} -> Effect (Collection t)
create o = runFn1 createImpl (FFI.toNullable o)

create' :: forall t . Effect (Collection t)
create' = runFn1 createImpl FFI.undefined

foreign import extendImpl :: forall t . Fn2 (Array t) (Collection t) (Collection t)

extend :: forall t . Array t -> Collection t -> Collection t
extend a c = runFn2 extendImpl a c
