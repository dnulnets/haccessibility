-- |
-- | The Geolocation module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Web.HTML.Navigator.Geolocation (NavigatorGeolocation,
  geolocation, Position(..),
  Coordinates (..),
  PositionError(..),
  PositionOptions(..),
  defaultOptions,
  getCurrentPosition,
  watchPosition,
  clearWatch) where

import Prelude (Unit)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff,
  fromEffectFnAff)

import Data.Nullable (Nullable)
import Data.Tuple (Tuple)

import Web.HTML.Navigator (Navigator)

{-|
  | fGetCurrentPosition is the foreign interface for
  | navigator.GetCurrentPosition
-}
foreign import _getCurrentPosition  :: PositionOptions
                                    -> NavigatorGeolocation
                                    -> EffectFnAff Position

-- | The FFI for `geolocation.watchPosition`
foreign import _watchPosition :: forall a b. (a->b->Tuple a b)
                              -> PositionOptions
                              -> (Tuple Int Position -> Effect Unit)
                              -> (Tuple Int PositionError -> Effect Unit)
                              -> NavigatorGeolocation
                              -> EffectFnAff Int

-- | The FFI for `geolocation.clearWatch`.
foreign import _clearWatch  :: Int
                            -> NavigatorGeolocation
                            -> Effect Unit

{-|
  | geolocation returns a geolocation object if the current browser supports
  | the Geolocation API
-}
foreign import geolocation :: Navigator -> Effect (Nullable NavigatorGeolocation)

{-|
  | NavigatorGeolocation is a dummy type for the geolocation object.
-}
foreign import data NavigatorGeolocation :: Type

-- | The PureScript interface to `navigator.getCurrentPosition`
getCurrentPosition
  :: PositionOptions
  -> NavigatorGeolocation
  -> Aff Position
getCurrentPosition po ngl = fromEffectFnAff (_getCurrentPosition po ngl)

-- | The PureScript interface to `navigator.watchPosition`
watchPosition
 :: forall a b . (a->b->Tuple a b) ->PositionOptions
  -> (Tuple Int Position -> Effect Unit)
  -> (Tuple Int PositionError -> Effect Unit)
  -> NavigatorGeolocation
  -> Aff Int
watchPosition t po fn fne ngl = fromEffectFnAff (_watchPosition t po fn fne ngl)

-- | The purescript interface to clearWatch
clearWatch :: Int -> NavigatorGeolocation -> Effect Unit
clearWatch = _clearWatch

-- | Coordinates is the datatype of the coordinates of a position.
-- | The geographic coordinate reference system used by the attributes in this
-- | interface is the World Geodetic System (2d).
type Coordinates = {
    latitude         :: Number,
    longitude        :: Number,
    altitude         :: Nullable Number,
    accuracy         :: Number,
    altitudeAccuracy :: Nullable Number,
    heading          :: Nullable Number,
    speed            :: Nullable Number
}

-- | The container for the geolocation information returned by this
-- | API.
type Position = {
    coords    :: Coordinates,
    timestamp :: Int
}

-- | The error which might be returned by
-- | `getCurrentPosition` and `watchPosition`.
type PositionError = {
    code    :: Int,
    message :: String
}

-- | The options for `getCurrentPosition` and
-- | `watchPosition`.
type PositionOptions = {
    enableHighAccuracy :: Boolean,
    timeout            :: Int,
    maximumAge         :: Int
}

-- | A PositionOptions with reasonable default values.
defaultOptions :: PositionOptions
defaultOptions = {
    enableHighAccuracy: true,
    timeout           : 5000,
    maximumAge        : 0
}
