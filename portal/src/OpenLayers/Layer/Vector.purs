-- |
-- | The OpenLayers Vector module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.Vector (
  Vector
  , Style(..)
  , RawVector

  , create
  , create'
  
  , setStyle ) where

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
import OpenLayers.Layer.BaseVectorLayer as BaseVectorLayer
import OpenLayers.Style.Style as Style
import OpenLayers.Feature as Feature
import OpenLayers.FFI as FFI

--
-- Our own data types
--
data Style =  Style Style.Style
            | StyleFunction (Feature.Feature->Number->Effect (Nullable Style.Style))
            | StyleArray (Array Style.Style)
--
-- Foreign data types
-- 
foreign import data RawVector :: Type
type Vector = BaseVectorLayer.BaseVectorLayer RawVector

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Vector)

create :: forall r . Maybe {|r} -> Effect Vector
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect Vector
create' = runFn1 createImpl FFI.undefined

--
-- Setters
--
foreign import setStyleImpl :: Fn2 Style.Style Vector (Effect Unit)
foreign import setStyleFImpl :: Fn2 (Feature.Feature->Number->Effect (Nullable Style.Style)) Vector (Effect Unit)
foreign import setStyleAImpl :: Fn2 (Array Style.Style) Vector (Effect Unit)

setStyle::Style->Vector->Effect Unit
setStyle (Style s) self = runFn2 setStyleImpl s self
setStyle (StyleFunction f) self = runFn2 setStyleFImpl f self
setStyle (StyleArray a) self = runFn2 setStyleAImpl a self