-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Feature (
  Feature
  , create

  , setStyle
  , setProperties
  , setGeometry
  
  , get) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , Fn3
  , Fn4
  , Fn5
  , runFn1
  , runFn2
  , runFn3
  , runFn4
  , runFn5)

-- Effect imports
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

--
-- Our own imports
--
import OpenLayers.Geom.Geometry as Geometry

--
-- Foreign data types
-- 
foreign import data Feature :: Type
instance showTile :: Show Feature where
  show _ = "Feature"

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Feature))

create :: forall r . {|r} -> Effect (Maybe Feature)
create o = toMaybe <$> runFn1 createImpl o

foreign import getImpl :: Fn2 String Feature (Effect (Nullable String))

--
-- setters
--
foreign import setStyleImpl :: forall r . Fn2 (Nullable {|r}) Feature (Effect Unit)
setStyle::forall r . Maybe {|r}->Feature->Effect Unit
setStyle r self = runFn2 setStyleImpl (toNullable r) self

foreign import setPropertiesImpl :: forall r . Fn2 {|r} Feature (Effect Unit)
setProperties::forall r . {|r} -> Feature -> Effect Unit
setProperties r self = runFn2 setPropertiesImpl r self

foreign import setGeometryImpl :: forall g . Fn2 (Nullable (Geometry.Geometry g)) Feature (Effect Unit)
setGeometry::forall g . Maybe (Geometry.Geometry g) -> Feature -> Effect Unit
setGeometry r self = runFn2 setGeometryImpl (toNullable r) self

--
-- getters
--
get :: String -> Feature -> Effect (Maybe String)
get n f = toMaybe <$> runFn2 getImpl n f
