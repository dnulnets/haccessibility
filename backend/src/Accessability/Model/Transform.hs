{-# LANGUAGE FlexibleContexts #-}

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
    toGQLItem,
    toGenericUser,
    toGenericAttribute,
    toGenericItemAttribute,
    textToKey,
    keyToText,
    idToKey,
    keyToID) where

--
-- Standard imports
--
import           Data.HexString               (fromBinary, hexString, toBinary,
                                               toText)
import           Data.Morpheus.Types          (ID (..), unpackID)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)

--
-- Persistence imports
--
import           Database.Persist.Sql

--
-- Our own imports
--
import           Accessability.Data.Geo
import qualified Accessability.Data.Item      as G
import qualified Accessability.Data.User      as G
import qualified Accessability.Model.Database as DB
import qualified Accessability.Model.GQL      as GQL

-- | Convert from ID to database key
idToKey:: ToBackendKey SqlBackend record => ID -> Key record
idToKey key = toSqlKey $ toBinary $ hexString $ encodeUtf8 $ unpackID key

keyToID::ToBackendKey SqlBackend record => Key record -> ID
keyToID key = ID { unpackID = toText $ fromBinary $ fromSqlKey key }

-- | Convert from Text to database key
textToKey::ToBackendKey SqlBackend record => Text -> Key record
textToKey key = toSqlKey $ toBinary $ hexString $ encodeUtf8 key

-- | Convert from Text to database key
keyToText::ToBackendKey SqlBackend record => Key record -> Text
keyToText key = toText $ fromBinary $ fromSqlKey key

-- | Converts a database attribute to a generic attribute
toGenericAttribute::(Key DB.Attribute, DB.Attribute)->G.Attribute
toGenericAttribute (key, a) = G.Attribute {
    G.attributeAttributeId = Just $ keyToText key,
    G.attributeItemId  = Nothing,
    G.attributeDescription = DB.attributeDescription a,
    G.attributeName = DB.attributeName a,
    G.attributeTypeof = DB.attributeTypeof a,
    G.attributeUnit = DB.attributeUnit  a,
    G.attributeValue = Nothing,
    G.attributeAttributeValueId = Nothing}

-- | Converts a database attribute to a generic attribute
toGenericItemAttribute::(Key DB.Attribute, DB.Attribute, Key DB.AttributeValue, DB.AttributeValue)->G.Attribute
toGenericItemAttribute (ka, a, kv, v) = G.Attribute {
    G.attributeAttributeId = Just $ keyToText ka,
    G.attributeItemId = Just $ keyToText (DB.attributeValueItem v),
    G.attributeDescription = DB.attributeDescription a,
    G.attributeName = DB.attributeName a,
    G.attributeTypeof = DB.attributeTypeof a,
    G.attributeUnit = DB.attributeUnit  a,
    G.attributeValue = Just $ DB.attributeValueValue v,
    G.attributeAttributeValueId = Just $ keyToText kv}

-- | Converts a database item to a GQL item
toGQLItem::(Key DB.Item, DB.Item, Maybe Double)  -- ^ The database item and distance
    ->GQL.Item      -- ^ The GQL item
toGQLItem (key, item, d) = GQL.Item { GQL.itemId = Just $ keyToID key,
    GQL.itemName =  DB.itemName item,
    GQL.itemGuid = DB.itemGuid item,
    GQL.itemModifier = DB.itemModifier item,
    GQL.itemApproval = DB.itemApproval item,
    GQL.itemCreated = DB.itemCreated item,
    GQL.itemDescription = DB.itemDescription item,
    GQL.itemSource = DB.itemSource item,
    GQL.itemDistance = realToFrac <$> d,
    GQL.itemLongitude = realToFrac $ longitude $ DB.itemPosition item,
    GQL.itemLatitude = realToFrac $ latitude $ DB.itemPosition item}

-- | Converts a database item to a generic
toGenericItem::(Key DB.Item, DB.Item, Maybe Double)   -- ^ The database item
    ->G.Item                            -- ^ The Generic item
toGenericItem (key, item, d) = G.Item { G.itemId = Just $ keyToText key,
    G.itemName =  DB.itemName item,
    G.itemGuid = DB.itemGuid item,
    G.itemCreated = DB.itemCreated item,
    G.itemModifier = DB.itemModifier item,
    G.itemApproval = DB.itemApproval item,
    G.itemDescription = DB.itemDescription item,
    G.itemSource = DB.itemSource item,
    G.itemDistance = realToFrac <$> d,
    G.itemLongitude = realToFrac $ longitude $ DB.itemPosition item,
    G.itemLatitude = realToFrac $ latitude $ DB.itemPosition item}

-- | Converts a GQL item to a database item
toDataItem::GQL.Item   -- ^ The database item
    ->DB.Item        -- ^ The GQL item
toDataItem item = DB.Item { DB.itemName =  GQL.itemName item,
    DB.itemGuid = GQL.itemGuid item,
    DB.itemModifier = GQL.itemModifier item,
    DB.itemApproval = GQL.itemApproval item,
    DB.itemCreated = GQL.itemCreated item,
    DB.itemDescription = GQL.itemDescription item,
    DB.itemSource = GQL.itemSource item,
    DB.itemPosition = Position $ PointXY (realToFrac $ GQL.itemLatitude item) (realToFrac $ GQL.itemLongitude item)}

-- | Converts a Database user to a generic user
toGenericUser::(Key DB.User, DB.User)->G.User
toGenericUser (k, u) = G.User {
        G.userId = Just $ keyToText k,
        G.userUsername = DB.userUsername u,
        G.userPassword = DB.userPassword u,
        G.userEmail = DB.userEmail u
    }