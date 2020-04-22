-- |
-- | The Openlayers module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Web.OL.Map where

import Prelude (Unit)

import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

import Data.Nullable (Nullable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

--
-- A coordinate
--

-- |The coordinate for a position with altitude and accuracy.
type Coordinates = {
    latitude         :: Maybe Number    -- ^The latitude
    , longitude        :: Maybe Number  -- ^The logitude
    , altitude         :: Maybe Number  -- ^The altidude in meters
    , accuracy         :: Maybe Number  -- ^The accuracy of the position
    , altitudeAccuracy :: Maybe Number  -- ^The accuracy of the altitude
}

--
-- The foreign functions and types
--
foreign import data OLMap :: Type
foreign import data OLGeolocation :: Type

--
-- Create Map
--

foreign import createMapImpl  :: Fn4 String Number Number Int (Effect (Nullable OLMap))

createMap   :: String   -- ^The element identity where the map is shown
            ->Number    -- ^Longitude
            ->Number    -- ^Latitude
            ->Int       -- ^Zoom level
            ->Effect (Nullable OLMap)   -- ^The map object            
createMap n lo la z = runFn4 createMapImpl n lo la z

--
-- Remove Target
--

foreign import removeTargetImpl :: Fn1 OLMap (Effect Unit)

-- |Removes the DOM element id as target for the map.
removeTarget    :: OLMap        -- ^The Map
                ->Effect Unit   -- ^Nothing to return
removeTarget m = runFn1 removeTargetImpl m 

--
-- Set Map Center
--

foreign import setCenterImpl::Fn3 OLMap Number Number (Effect Unit)

-- |Sets the center of the map to the latitude and longitude provided.
setCenter   :: OLMap -- ^The map
            ->Number  -- ^Longitude
            ->Number  -- ^Latitude
            ->Effect Unit
setCenter m lo la = runFn3 setCenterImpl m lo la

--
-- Add Geolocation To Map
--

foreign import addGeolocationToMapImpl :: Fn1 OLMap (Effect (Nullable OLGeolocation))

-- |Adds a geolocation device and automatic update of a position icon on the map.
-- |It needs to be activated with a call to setTracking.
addGeolocationToMap :: OLMap                            -- ^The map
                    -> Effect (Nullable OLGeolocation)  -- ^The geolocation device attached to the map
addGeolocationToMap m = runFn1 addGeolocationToMapImpl m

--
-- Set the tracking on or off
--

foreign import setTrackingImpl :: Fn2 OLGeolocation Boolean (Effect Unit)

-- |Activates or deactivates the tracking of the geolocation device.
setTracking :: OLGeolocation    -- ^The geolocation device
            -> Boolean          -- ^Turn tracking on (true) or off (false)
            -> Effect Unit
setTracking gl b = runFn2 setTrackingImpl gl b

--
-- Get the current coordinates
--

foreign import getCoordinatesImpl::forall a. Fn3 (a -> Maybe a) (Maybe a) OLGeolocation (Effect Coordinates)

-- |Returns with the current position of the geolocation device.
getCoordinates  :: OLGeolocation        -- ^The geolocation device
                -> Effect Coordinates   -- ^The coordinates
getCoordinates gl = runFn3 getCoordinatesImpl Just Nothing gl

--
-- Debug writer
--
foreign import debugWriteImpl:: Fn1 OLMap (Effect Unit)

debugWrite  ::OLMap
            -> Effect Unit
debugWrite m = runFn1 debugWriteImpl m
