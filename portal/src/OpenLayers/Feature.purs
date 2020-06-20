-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Feature (
  module Object
  , module Observable

  , Feature
  , RawFeature
  , create
  , create'

  , setStyle
  , setProperties
  , setGeometry
  , setGeometry') where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , runFn1
  , runFn2)

-- Effect imports
import Effect (Effect)

--
-- Our own imports
--
import OpenLayers.FFI as FFI
import OpenLayers.Geom.Geometry as Geometry
import OpenLayers.Style.Style as Style
import OpenLayers.Object (BaseObject, get) as Object
import OpenLayers.Observable (on, once, un) as Observable
--
-- Foreign data types
-- 
foreign import data RawFeature :: Type
type Feature = Object.BaseObject RawFeature

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Feature)

create :: forall r . Maybe {|r} -> Effect Feature
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect Feature
create' = runFn1 createImpl FFI.undefined

--
-- setters
--
foreign import setStyleImpl :: Fn2 (Nullable Style.Style) Feature (Effect Unit)
setStyle::Maybe Style.Style->Feature->Effect Unit
setStyle st self = runFn2 setStyleImpl (toNullable st) self

foreign import setPropertiesImpl :: forall r . Fn2 {|r} Feature (Effect Unit)
setProperties::forall r . {|r} -> Feature -> Effect Unit
setProperties r self = runFn2 setPropertiesImpl r self

foreign import setGeometryImpl :: forall g . Fn2 (FFI.NullableOrUndefined (Geometry.Geometry g)) Feature (Effect Unit)

setGeometry::forall g . Geometry.Geometry g -> Feature -> Effect Unit
setGeometry r self = runFn2 setGeometryImpl (FFI.notNullOrUndefined r) self

setGeometry'::Feature -> Effect Unit
setGeometry' self = runFn2 setGeometryImpl FFI.undefined self
