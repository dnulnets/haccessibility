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
    Geo(..),
    Position(..),
    longitude,
    latitude,
    position,
    PointXY(..)) where

--
-- Import standard libs
--
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Char8 (pack)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Geospatial (GeospatialGeometry(..), GeoPositionWithoutCRS(..), PointXY(..), GeoPoint(..)) 

--
-- To be able to generate the persist field
--
import Database.Persist
import Database.Persist.Sql

--import Data.Geospatial
import Data.Hex
import Data.HexString
import Data.Internal.Ewkb.Geometry (SridType(..))
import Data.Internal.Wkb.Endian
import Data.Ewkb

--
-- New data types
--
-- |Our default SRID, which is WGS84, 4326
defaultSRID = Srid 4326

-- | The geography type
newtype Geo = Geo GeospatialGeometry
    deriving (Show)

-- | The Position type
newtype Position = Position PointXY
  deriving (Show)

--
-- Convenience functions
--
longitude::Position->Double
longitude (Position (PointXY x _)) = x

latitude::Position->Double
latitude (Position (PointXY _ y)) = y

position::Maybe Double->Maybe Double->Maybe Position
position (Just lo) (Just la) = Just (Position (PointXY lo la))
position _ _ = Nothing


--
-- Persistence
--
geoPoint::GeospatialGeometry->Either String PointXY
geoPoint (Point (GeoPoint (GeoPointXY pxy))) = Right pxy
geoPoint _ = Left "The geospatialgeometry is not a POINT"

-- |Marshalling to and from the SQL data type and the Position data type
instance PersistField Position where
  toPersistValue (Position t) = PersistDbSpecific $ encodeUtf8 $ toText $ fromBytes $ toStrict $ toByteString LittleEndian defaultSRID (Point (GeoPoint (GeoPointXY t)))

  fromPersistValue (PersistDbSpecific t) = do
    p <- return $ geoPoint <$> (parseHexByteString (Hex t))
    case p of
      Left e -> Left $ Data.Text.pack e
      Right (Left ie) -> Left $ Data.Text.pack ie
      Right (Right v) -> Right $ Position v
    
--    either (Left . pack) Right $ (Position . geoPoint) <$> parseHexByteString (Hex t)
  fromPersistValue _ = Left "Position values must be converted from PersistDbSpecific"

-- |Set the SQL data type for the Position type
instance PersistFieldSql Position where
  sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"

-- |Marshalling to and from the SQL data type and the Geo data type
instance PersistField Geo where
  toPersistValue (Geo t) = PersistDbSpecific $ toStrict $ toByteString LittleEndian defaultSRID t

  fromPersistValue (PersistDbSpecific t) = either (Left . Data.Text.pack) Right $ Geo <$> parseHexByteString (Hex t)
  fromPersistValue _ = Left "Geo values must be converted from PersistDbSpecific"

-- |Set the SQL data type for the geography point
instance PersistFieldSql Geo where
  sqlType _ = SqlOther "GEOGRAPHY"
