-- |
-- | The OpenLayers View module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.View (
  View

  , create
  , create'

  , setCenter

  , getProjection) where

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

-- Our own imports
import OpenLayers.FFI as FFI

--
-- Foreign data types
-- 
foreign import data View :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect View)

create :: forall r . Maybe {|r} -> Effect View
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect View
create' = runFn1 createImpl FFI.undefined

--
-- set functions
--
foreign import setCenterImpl :: Fn2(Array Number) View (Effect Unit)

setCenter :: Array Number -> View -> Effect Unit
setCenter pos self = runFn2 setCenterImpl pos self

--
-- get functins
--
foreign import getProjectionImpl :: Fn1 View (Effect (Nullable String))

getProjection :: View -> Effect (Maybe String)
getProjection self = toMaybe <$> runFn1 getProjectionImpl self
