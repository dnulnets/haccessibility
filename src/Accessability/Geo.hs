{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}

module Accessability.Geo (
  
  -- Own stuff
  GeodeticPosition(..), 

  -- Reexporting
  Geodetic(..),
  WGS84 (..),
  degree, meter,
  (*~))
  
  where

--
-- Import standard libs
--
--import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text, pack)

import GHC.Generics ( Generic )

--
-- Geodetics
--
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits

import Geodetics.Ellipsoids (Ellipsoid(..))
import Geodetics.Geodetic (WGS84(..), Geodetic (..))

--
-- Import for morpheus
--
import Data.Morpheus.Kind     (SCALAR)
import Data.Morpheus          (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types    (GQLRootResolver (..),
                              IORes, 
                              Undefined(..),
                              GQLType(..),
                              liftEitherM,
                              constRes,
                              Res,
                              GQLScalar(..), ScalarValue(..))

--
-- Our internal schema
--

type GeodeticPosition = Geodetic WGS84

instance GQLScalar GeodeticPosition where
  parseValue (String x) = pure $ Geodetic {
    latitude=62.39129 *~ degree, 
    longitude=17.3063 *~ degree,
    geoAlt=0.0 *~ meter,
    ellipsoid=WGS84}
  parseValue _ = Left "Geodetic position must be a string of format 62° 23' 28.64\" N, 17° 18' 22.68\" E, 0.0 m WGS84"
  serialize p = String $ pack $ show  p

instance GQLType GeodeticPosition where
  type KIND GeodeticPosition = SCALAR
  description = const $ Just $ "A WGS84 longitude, latitude, height scalar of format 62° 23' 28.64\" N, 17° 18' 22.68\" E, 0.0 m WGS84"
