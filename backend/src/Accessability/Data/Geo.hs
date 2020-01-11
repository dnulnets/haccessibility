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
module Accessability.Data.Geo (
    Geo(..), 
    toPoint) where

--
-- Import standard libs
--
import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (pack)

--
-- To be bale to generate the persist field
--
import Database.Persist
import Database.Persist.TH
import Database.Persist.Class
import Database.Persist.Sql

-- |
--

data Geo = Geo ByteString
    deriving (Show)

instance PersistField Geo where
  toPersistValue (Geo t) = PersistDbSpecific t

  fromPersistValue (PersistDbSpecific t) = Right $ Geo $ Data.ByteString.concat ["'", t, "'"]
  fromPersistValue _ = Left "Geo values must be converted from PersistDbSpecific"

instance PersistFieldSql Geo where
  sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"

toPoint :: Double -> Double -> Geo
toPoint lat lon = Geo $ Data.ByteString.concat ["'POINT(", ps $ lon, " ", ps $ lat, ")'"]
  where ps = pack . show
