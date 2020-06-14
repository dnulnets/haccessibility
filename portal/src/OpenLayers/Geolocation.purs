-- |
-- | The OpenLayers Geolocation module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geolocation (
    Geolocation
    , create

    , ChangeAccuracyGeometryEvent
    , ChangePositionEvent
    , ErrorEvent
    , Key

    , setTracking
    
    , getAccuracyGeometry
    , getPosition

    , onChangeAccuracyGeometry
    , onChangePosition
    , onError

    , onceChangePosition

    , unChangeAccuracyGeometry
    , unChangePosition
    , unError
    ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , Fn3
  , runFn1
  , runFn2
  , runFn3)

-- Effect imports
import Effect (Effect)

-- Own imports
import OpenLayers.Geom.Polygon as Polygon

--
-- Foreign data types
-- 
foreign import data Geolocation :: Type
foreign import data Key :: Type

instance showGeolocation :: Show Geolocation where
  show _ = "Geolocation"

--
-- Data types
--

-- Geolocation events

type ErrorEvent = {}
type ChangeAccuracyGeometryEvent = {}
type ChangePositionEvent = {}

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Geolocation))

create :: forall r . {|r} -> Effect (Maybe Geolocation)
create o = toMaybe <$> runFn1 createImpl o

--
-- Setters
--

foreign import setTrackingImpl :: Fn2 Boolean Geolocation (Effect Unit)
setTracking::Boolean->Geolocation->Effect Unit
setTracking onoff g = runFn2 setTrackingImpl onoff g

--
-- Getters
--
foreign import getAccuracyGeometryImpl :: Fn1 Geolocation (Effect (Nullable Polygon.Polygon))
getAccuracyGeometry :: Geolocation -> Effect (Maybe Polygon.Polygon)
getAccuracyGeometry self = toMaybe <$> runFn1 getAccuracyGeometryImpl self

foreign import getPositionImpl :: Fn1 Geolocation (Effect (Nullable (Array Number)))
getPosition :: Geolocation -> Effect (Maybe (Array Number))
getPosition self = toMaybe <$> runFn1 getPositionImpl self

foreign import getAltitudeImpl :: Fn1 Geolocation (Effect (Nullable Number))
getAltitude :: Geolocation -> Effect (Maybe Number)
getAltitude self = toMaybe <$> runFn1 getAltitudeImpl self

foreign import getAltitudeAccuracyImpl :: Fn1 Geolocation (Effect (Nullable Number))
getAltitudeAccuracy :: Geolocation -> Effect (Maybe Number)
getAltitudeAccuracy self = toMaybe <$> runFn1 getAltitudeAccuracyImpl self

foreign import getAccuracyImpl :: Fn1 Geolocation (Effect (Nullable Number))
getAccuracy :: Geolocation -> Effect (Maybe Number)
getAccuracy self = toMaybe <$> runFn1 getAccuracyImpl self

--
-- Event handlers setup
--
--
-- All on_ functions
--
foreign import onImpl :: forall v a. Fn3 String (v -> Effect a) Geolocation (Effect Key)
foreign import onceImpl :: forall v a. Fn3 String (v -> Effect a) Geolocation (Effect Key)

onError :: (ErrorEvent -> Effect Unit) -> Geolocation -> Effect Key
onError fn self = runFn3 onImpl "error" fn self

onChangeAccuracyGeometry :: (ChangeAccuracyGeometryEvent-> Effect Unit) -> Geolocation -> Effect Key
onChangeAccuracyGeometry fn self = runFn3 onImpl "change:accuracyGeometry" fn self

onChangePosition :: (ChangePositionEvent -> Effect Unit) -> Geolocation -> Effect Key
onChangePosition fn self = runFn3 onImpl "change:position" fn self

onceChangePosition :: (ChangePositionEvent -> Effect Unit) -> Geolocation -> Effect Key
onceChangePosition fn self = runFn3 onceImpl "change:position" fn self
--
-- All un_ functions
--
foreign import unImpl :: Fn3 String Key Geolocation (Effect Unit)

unError :: Key -> Geolocation -> Effect Unit
unError key self = runFn3 unImpl "error" key self

unChangeAccuracyGeometry :: Key -> Geolocation -> Effect Unit
unChangeAccuracyGeometry key self = runFn3 unImpl "change:accuracyGeometry" key self

unChangePosition :: Key -> Geolocation -> Effect Unit
unChangePosition key self = runFn3 unImpl "change:position" key self
