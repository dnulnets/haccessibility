{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TemplateHaskell      #-}
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
-- This module creates the persist field and sql definition for the geo type in postgresql
--
module Accessability.Data.Persist.Geo (
    toPersistValue,
    fromPersistValue,
    sqlType) where

--
-- Import standard libs
--
import Data.ByteString (ByteString, concat)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text(..), pack)

--
-- To be able to generate the persist field
--
import Database.Persist
import Database.Persist.TH
import Database.Persist.Class
import Database.Persist.Sql

import Data.Geospatial
import Data.Hex
import Data.Internal.Ewkb.Geometry
import Data.Internal.Wkb.Endian
import Data.Ewkb

import Accessability.Data.Geo

instance PersistField Geo where
  toPersistValue (Geo t) = PersistDbSpecific $ toStrict $ toByteString LittleEndian (Srid 3226) t

  fromPersistValue (PersistDbSpecific t) = either (Left . pack) Right $ Geo <$> (parseHexByteString $ Hex t)
  fromPersistValue _ = Left "Geo values must be converted from PersistDbSpecific"

instance PersistFieldSql Geo where
  sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"
