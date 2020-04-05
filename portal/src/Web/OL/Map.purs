-- |
-- | The Openlayers module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Web.OL.Map where

import Prelude (Unit)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

import Data.Nullable (Nullable)
import Data.Tuple (Tuple)

--
-- The foreign functions and types
--
foreign import data OLMap :: Type
foreign import data OLGeolocation :: Type

foreign import createMap  :: String -- ^Name of the element where it should be rendered
                          ->Number  -- ^Longitude
                          ->Number  -- ^Latitude
                          ->Int     -- ^Zoom level
                          ->Effect (Nullable OLMap) -- ^The Map

foreign import removeTarget :: OLMap -- ^The Map
                            ->Effect Unit -- ^Nothing to return

foreign import setCenter  :: OLMap -- ^The map
                          ->Number  -- ^Longitude
                          ->Number  -- ^Latitude
                          ->Effect Unit

foreign import addGeolocationToMap :: OLMap -- ^The map
                            -> Effect (Nullable OLGeolocation) -- ^The geolocation device attached to the map

foreign import setTracking :: OLGeolocation -- ^The geolocation device
                            -> Boolean      -- ^Turn tracking on (true) or off (false)
                            -> Effect Unit
