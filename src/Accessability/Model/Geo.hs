{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      : Accessability.Model.Geo
-- Description : The geodetic data type
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module creates the definition of the type GeoideticPosition that can be used in a
-- graphql query, persist for databases and is represented in haskell with Geodetic WGS84
--
module Accessability.Model.Geo (
  
  -- Own stuff
  GeodeticPosition(..), 

  -- Reexporting
  Geodetic(..), show, readsPrec,
  WGS84 (..),
  degree, meter,
  (*~))
  
  where

--
-- Import standard libs
--
import Data.Text (Text, pack)

--
-- Geodetics
--
import Geodetics.Ellipsoids (Ellipsoid(..))
import Geodetics.Geodetic (WGS84(..), Geodetic (..), showAngle)
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits

--
-- To be bale to generate the persist field
--
import Database.Persist
import Database.Persist.TH

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
                              constRes,
                              Res,
                              GQLScalar(..), ScalarValue(..))

-- | Definition of Geodetic position in WGS84
data GeodeticPosition = Position (Geodetic WGS84)

-- Make it possible to store this in the database
derivePersistField "GeodeticPosition"

-- | Conversion from Geodetic to string
instance Show GeodeticPosition where
  show (Position p) = concat [
    show $ (latitude p) /~ degree,  ";",
    show $ (longitude p) /~ degree]

-- | Conversion from string to Geodetic
instance Read GeodeticPosition where
  readsPrec p s = do
    (lat, s1) <- (readsPrec p s)::[(Double,String)]
    (";", s2) <- lex s1
    (lon, s3) <- (readsPrec p s2)::[(Double,String)]
    return (Position Geodetic {
          latitude=lat *~ degree,   -- Latitude in degrees
          longitude=lon *~ degree,  -- Longitude in degrees
          geoAlt=0.0 *~ meter,      -- Always 0.0 for now
          ellipsoid=WGS84}, s3)     -- Always WGS84

-- | Make the GeodeticPosition a scalar
instance GQLScalar GeodeticPosition where

  -- | Parse the value for a graphql string to scalar
  parseValue (String x) = pure $ Position Geodetic {
    latitude=62.39129 *~ degree, 
    longitude=17.3063 *~ degree,
    geoAlt=0.0 *~ meter,
    ellipsoid=WGS84}
  parseValue _ = Left "Geodetic position must be in compressed degrees with decimals in WGS84 of format <latitude>;<longitude>"
  
  -- | Serialize the data to a graphql scalar string
  serialize p = String $ pack $ show  p

-- | Make the GeodeticPosition a type of kind scalar
instance GQLType GeodeticPosition where
  type KIND GeodeticPosition = SCALAR
  description = const $ Just $ "This holds a geodetic position and must be in compressed degrees with decimals in WGS84 of format <latitude>;<longitude>"
