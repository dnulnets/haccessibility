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
import OpenLayers.Geom.Geometry as Geometry
import OpenLayers.Style.Style as Style

--
-- Foreign data types
-- 
foreign import data Feature :: Type

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
foreign import setStyleImpl :: Fn2 (Nullable Style.Style) Feature (Effect Unit)
setStyle::Maybe Style.Style->Feature->Effect Unit
setStyle st self = runFn2 setStyleImpl (toNullable st) self

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
