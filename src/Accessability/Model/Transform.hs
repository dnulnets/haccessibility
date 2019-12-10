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
    toGenericItem,
    toGQLItem) where

import Data.Char (toLower)
import Data.Text (Text, pack)
import Data.Morpheus.Types    (ID(..))
import Database.Persist.Sql

import qualified Accessability.Model.DB as DB
import qualified Accessability.Model.GQL as GQL
import qualified Accessability.Model.Generic as G

-- | Converts a database item to a GQL item
toGQLItem::(Key DB.Item, DB.Item)  -- ^ The database item
    ->GQL.Item      -- ^ The GQL item
toGQLItem (key, item) = GQL.Item { GQL.itemID = Just $ ID {unpackID = pack $ show $ fromSqlKey key },
    GQL.itemName =  DB.itemName item,
    GQL.itemDescription = DB.itemDescription item,
    GQL.itemLevel = DB.itemLevel item,
    GQL.itemSource = DB.itemSource item,
    GQL.itemState = DB.itemState item,
    GQL.itemLongitude = realToFrac $ DB.itemLongitude item,
    GQL.itemLatitude = realToFrac $ DB.itemLatitude item}

-- | Converts a database item to a generic
toGenericItem::(Key DB.Item, DB.Item)   -- ^ The database item
    ->G.Item                            -- ^ The Generic item
toGenericItem (key, item) = G.Item { G.itemID = Just $ pack $ show $ fromSqlKey key,
    G.itemName =  DB.itemName item,
    G.itemDescription = DB.itemDescription item,
    G.itemLevel = DB.itemLevel item,
    G.itemSource = DB.itemSource item,
    G.itemState = DB.itemState item,
    G.itemLongitude = realToFrac $ DB.itemLongitude item,
    G.itemLatitude = realToFrac $ DB.itemLatitude item}

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
