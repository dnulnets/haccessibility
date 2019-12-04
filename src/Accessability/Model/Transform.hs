-- |
-- Module      : Acessability.Model.Transform
-- Description : The database model
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains functions that transform from database to gql primitives
--
module Accessability.Model.Transform (
    toDataItem,
    toGQLItem) where

import Data.Text (Text, pack)
import Data.Morpheus.Types    (ID(..))
import Database.Persist.Sql

import qualified Accessability.Model.Data as DB
import qualified Accessability.Model.GQL as GQL

-- | Converts a database item to a GQL item
toGQLItem::Key DB.Item -> DB.Item  -- ^ The database item
    ->GQL.Item      -- ^ The GQL item
toGQLItem key item = GQL.Item { GQL.itemID = Just $ ID {unpackID = pack $ show $ fromSqlKey key },
    GQL.itemName =  DB.itemName item,
    GQL.itemDescription = DB.itemDescription item,
    GQL.itemLevel = DB.itemLevel item,
    GQL.itemSource = DB.itemSource item,
    GQL.itemState = DB.itemState item,
    GQL.itemLongitude = realToFrac $ DB.itemLongitude item,
    GQL.itemLatitude = realToFrac $ DB.itemLatitude item}

-- | Converts a GQL item to a database item
toDataItem::GQL.Item   -- ^ The database item
    ->DB.Item        -- ^ The GQL item
toDataItem item = DB.Item { DB.itemName =  GQL.itemName item,
    DB.itemDescription = GQL.itemDescription item,
    DB.itemLevel = GQL.itemLevel item,
    DB.itemSource = GQL.itemSource item,
    DB.itemState = GQL.itemState item,
    DB.itemLatitude = realToFrac $ GQL.itemLatitude item,
    DB.itemLongitude = realToFrac $ GQL.itemLongitude item}
