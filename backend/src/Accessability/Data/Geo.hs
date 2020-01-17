{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}

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
    Geo(..)) where

--
-- Import standard libs
--
import Data.ByteString (ByteString, concat)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text(..), pack)
import Data.Geospatial (GeospatialGeometry)

-- | The geography type
data Geo = Geo GeospatialGeometry
    deriving (Show)