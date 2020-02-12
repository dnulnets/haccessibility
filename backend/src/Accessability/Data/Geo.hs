{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Accessability.Data.Geo
-- Description : The geodetic data type
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module creates a new geography type containing generic geospatial content
-- but also specific positional type as well. It adds Persistent support for PostGIS extension
-- in postgres using Extended Well Known Binary format, EWKB
--
module Accessability.Data.Geo (
    Geospatial(..),
    GeospatialPosition(..),
    longitude,
    latitude,
    maybePosition,
    position,
    PointXY(..)) where

--
-- Import standard libs
--
import           Data.ByteString.Lazy        (toStrict)
import           Data.Geospatial             (GeoPoint (..),
                                              GeoPositionWithoutCRS (..),
                                              GeospatialGeometry (..),
                                              PointXY (..))
import           Data.Text                   (pack)
import           Data.Text.Encoding          (encodeUtf8)

--
-- To be able to generate the persist field
--
import           Database.Persist
import           Database.Persist.Sql

--import Data.Geospatial
import           Data.Ewkb
import           Data.Hex
import           Data.HexString
import           Data.Internal.Ewkb.Geometry (SridType (..))
import           Data.Internal.Wkb.Endian

--
-- New data types
--
-- |Our default SRID, which is WGS84, 4326
defaultSRID::SridType
defaultSRID = Srid 4326

-- | The generic Geography type
newtype Geospatial = Geometry GeospatialGeometry
  deriving (Show)

-- | The specific GeometryPoint, handles  type, X=longitude, Y=latitude
newtype GeospatialPosition = Position PointXY
  deriving (Show)

--
-- Convenience functions
--

-- |Extracts the longitude from a GeospatialPosition
longitude::GeospatialPosition -- ^ The geospatial position
  ->Double                    -- ^ The extracted longitude
longitude (Position (PointXY x _)) = x

latitude::GeospatialPosition  -- ^ The geospatial position
  ->Double                    -- ^ The extracted latitude
latitude (Position (PointXY _ y)) = y

maybePosition::Maybe Double    -- ^ Longitude
  ->Maybe Double          -- ^ Latitude
  ->Maybe GeospatialPosition  -- ^ The geosatial position
maybePosition (Just lo) (Just la) = Just (Position (PointXY lo la))
maybePosition _ _                 = Nothing

position::Double    -- ^ Longitude
  ->Double          -- ^ Latitude
  ->GeospatialPosition  -- ^ The geosatial position
position lo la = Position (PointXY lo la)

--
-- Persistence
--

-- |Extracts a PointXY from a GeospatialGeometry if it exists
geoPoint::GeospatialGeometry -- ^ The geospatial gemoetry
  ->Either String PointXY    -- ^ The PointXY or an error
geoPoint (Point (GeoPoint (GeoPointXY pxy))) = Right pxy
geoPoint _ = Left "The geospatialgeometry is not a POINTXY"

-- |Marshalling to and from the SQL data type and the Position data type
instance PersistField GeospatialPosition where
  toPersistValue (Position t) = PersistDbSpecific $ encodeUtf8 $ toText $ fromBytes $ toStrict $ toByteString LittleEndian defaultSRID (Point (GeoPoint (GeoPointXY t)))

  fromPersistValue (PersistDbSpecific t) = do
    let p = geoPoint <$> parseHexByteString (Hex t)
    case p of
      Left e          -> Left $ Data.Text.pack e
      Right (Left ie) -> Left $ Data.Text.pack ie
      Right (Right v) -> Right $ Position v

--    either (Left . pack) Right $ (Position . geoPoint) <$> parseHexByteString (Hex t)
  fromPersistValue _ = Left "Position values must be converted from PersistDbSpecific"

-- |Set the SQL data type for the Position type
instance PersistFieldSql GeospatialPosition where
  sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"

-- |Marshalling to and from the SQL data type and the Geo data type
instance PersistField Geospatial where
  toPersistValue (Geometry t) = PersistDbSpecific $ toStrict $ toByteString LittleEndian defaultSRID t

  fromPersistValue (PersistDbSpecific t) = either (Left . Data.Text.pack) Right $ Geometry <$> parseHexByteString (Hex t)
  fromPersistValue _ = Left "Geo values must be converted from PersistDbSpecific"

-- |Set the SQL data type for the geography point
instance PersistFieldSql Geospatial where
  sqlType _ = SqlOther "GEOGRAPHY"
