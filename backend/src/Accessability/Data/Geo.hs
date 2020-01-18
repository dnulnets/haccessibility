{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

-- |
-- Module      : Accessability.Data.Geo
-- Description : The geodetic data type
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module creates a new geography type
--
module Accessability.Data.Geo (
    Geo(..)) where

--
-- Import standard libs
--
import Data.ByteString.Lazy (toStrict)
import Data.Text (pack)
import Data.Geospatial (GeospatialGeometry)

--
-- To be able to generate the persist field
--
import Database.Persist
import Database.Persist.Sql

--import Data.Geospatial
import Data.Hex
import Data.Internal.Ewkb.Geometry
import Data.Internal.Wkb.Endian
import Data.Ewkb

--
-- New data types
--

-- | The geography type
newtype Geo = Geo GeospatialGeometry
    deriving (Show)

--
-- Persistence
--

-- |Marshalling to and from the SQL data type and the Geo data type
instance PersistField Geo where
  toPersistValue (Geo t) = PersistDbSpecific $ toStrict $ toByteString LittleEndian (Srid 3226) t

  fromPersistValue (PersistDbSpecific t) = either (Left . pack) Right $ Geo <$> parseHexByteString (Hex t)
  fromPersistValue _ = Left "Geo values must be converted from PersistDbSpecific"

-- |Set the SQL data type for the geography point
instance PersistFieldSql Geo where
  sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"
