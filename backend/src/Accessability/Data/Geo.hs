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
-- This module creates a new geography type
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
newtype Geo = Geo GeospatialGeometry
    deriving (Show)