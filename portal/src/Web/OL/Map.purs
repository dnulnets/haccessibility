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
foreign import createMap  :: String -- ^Name of the element where it should be rendered
                          ->Number  -- ^Longitude
                          ->Number  -- ^Latitude
                          ->Effect (Nullable OLMap) -- ^The Map
