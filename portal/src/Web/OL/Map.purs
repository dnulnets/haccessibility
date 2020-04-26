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
type Coordinate = {
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
foreign import data OLLayer :: Type

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
-- Get the current Coordinate
--

foreign import getCoordinateImpl::forall a. Fn3 (a -> Maybe a) (Maybe a) OLGeolocation (Effect Coordinate)

-- |Returns with the current position of the geolocation device.
getCoordinate  :: OLGeolocation        -- ^The geolocation device
                -> Effect Coordinate   -- ^The Coordinate
getCoordinate gl = runFn3 getCoordinateImpl Just Nothing gl

--
-- Debug writer
--
foreign import debugWriteImpl:: Fn1 OLMap (Effect Unit)

debugWrite  ::OLMap
            -> Effect Unit
debugWrite m = runFn1 debugWriteImpl m

--
-- Add one additional layer
--

foreign import addLayerToMapImpl :: Fn2 OLMap OLLayer (Effect Unit)

-- |Adds a layer to the map
addLayerToMap    :: OLMap        -- ^The Map
                -> OLLayer       -- ^The layer to add to the map
                ->Effect Unit   -- ^Nothing to return
addLayerToMap m l = runFn2 addLayerToMapImpl m l 

--
-- Remove a layer
--

foreign import removeLayerFromMapImpl :: Fn2 OLMap OLLayer (Effect Unit)

-- |Removes the layer from the map
removeLayerFromMap    :: OLMap    -- ^The Map
                -> OLLayer      -- ^The layer to remove from the map
                ->Effect Unit   -- ^Nothing to return
removeLayerFromMap m l = runFn2 removeLayerFromMapImpl m l 

--
-- Create the POI Layer
--

foreign import createPOILayerImpl :: forall p . Fn2 String (Array { longitude::Number,latitude::Number,name::String | p }) OLLayer

-- |Removes the DOM element id as target for the map.
createPOILayer  :: forall p . String         -- ^The map guid
                -> Array { longitude::Number,latitude::Number,name::String | p }        -- ^The list of POI:s
                -> OLLayer          -- ^The returned layer
createPOILayer guid pois = runFn2 createPOILayerImpl guid pois
