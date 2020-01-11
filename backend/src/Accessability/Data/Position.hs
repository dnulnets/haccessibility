{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}

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
module Accessability.Data.Position (
  
  -- Own stuff
  Position(..), 

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
import Database.Persist.Class
import Database.Persist.Sql


-- | Definition of Geodetic position in WGS84
data Position = Position (Geodetic WGS84)

-- | Conversion from Geodetic to string
instance Show Position where
  show (Position p) = concat [
    show $ (latitude p) /~ degree,  ";",
    show $ (longitude p) /~ degree]

-- | Conversion from string to Geodetic
instance Read Position where
  readsPrec p s = do
    (lat, s1) <- (readsPrec p s)::[(Double,String)]
    (";", s2) <- lex s1
    (lon, s3) <- (readsPrec p s2)::[(Double,String)]
    return (Position Geodetic {
          latitude=lat *~ degree,   -- Latitude in degrees
          longitude=lon *~ degree,  -- Longitude in degrees
          geoAlt=0.0 *~ meter,      -- Always 0.0 for now
          ellipsoid=WGS84}, s3)     -- Always WGS84

