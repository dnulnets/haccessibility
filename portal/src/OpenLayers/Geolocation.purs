-- |
-- | The OpenLayers Geolocation module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Geolocation (
    module Object
    , module Observable

    , Geolocation
    , RawGeolocation
    , create

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
import OpenLayers.FFI as FFI
import OpenLayers.Object(BaseObject, ObjectEvent) as Object
import OpenLayers.Observable (on, un, once) as Observable
import OpenLayers.Events (EventsKey, ListenerFunction) as Events
import OpenLayers.Events.Event (BaseEvent) as Event

--
-- Foreign data types
-- 
foreign import data RawGeolocation :: Type
type Geolocation = Object.BaseObject RawGeolocation

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

foreign import getPositionImpl :: Fn1 Geolocation (Effect (FFI.NullableOrUndefined (Array Number)))
getPosition :: Geolocation -> Effect (Maybe (Array Number))
getPosition self = FFI.toMaybe <$> runFn1 getPositionImpl self

foreign import getAltitudeImpl :: Fn1 Geolocation (Effect (FFI.NullableOrUndefined Number))
getAltitude :: Geolocation -> Effect (Maybe Number)
getAltitude self = FFI.toMaybe <$> runFn1 getAltitudeImpl self

foreign import getAltitudeAccuracyImpl :: Fn1 Geolocation (Effect (FFI.NullableOrUndefined Number))
getAltitudeAccuracy :: Geolocation -> Effect (Maybe Number)
getAltitudeAccuracy self = FFI.toMaybe <$> runFn1 getAltitudeAccuracyImpl self

foreign import getAccuracyImpl :: Fn1 Geolocation (Effect (FFI.NullableOrUndefined Number))
getAccuracy :: Geolocation -> Effect (Maybe Number)
getAccuracy self = FFI.toMaybe <$> runFn1 getAccuracyImpl self

--
-- Event handlers setup
--
--
-- All on_ functions
--
--foreign import onImpl :: forall v a. Fn3 String (v -> Effect a) Geolocation (Effect Key)
--foreign import onceImpl :: forall v a. Fn3 String (v -> Effect a) Geolocation (Effect Key)

onError :: forall e . Events.ListenerFunction (Event.BaseEvent e) -> Geolocation -> Effect Events.EventsKey
onError fn self = Observable.on "error" fn self

onChangeAccuracyGeometry :: forall e . Events.ListenerFunction (Object.ObjectEvent e) -> Geolocation -> Effect Events.EventsKey
onChangeAccuracyGeometry fn self = Observable.on "change:accuracyGeometry" fn self

onChangePosition :: forall e . Events.ListenerFunction (Object.ObjectEvent e) -> Geolocation -> Effect Events.EventsKey
onChangePosition fn self = Observable.on "change:position" fn self

onceChangePosition :: forall e . Events.ListenerFunction (Object.ObjectEvent e) -> Geolocation -> Effect Events.EventsKey
onceChangePosition fn self = Observable.once "change:position" fn self
--
-- All un_ functions
--
--foreign import unImpl :: Fn3 String Key Geolocation (Effect Unit)

unError :: Events.EventsKey -> Geolocation -> Effect Unit
unError key self = Observable.un "error" key self

unChangeAccuracyGeometry :: Events.EventsKey -> Geolocation -> Effect Unit
unChangeAccuracyGeometry key self = Observable.un "change:accuracyGeometry" key self

unChangePosition :: Events.EventsKey -> Geolocation -> Effect Unit
unChangePosition key self = Observable.un "change:position" key self
