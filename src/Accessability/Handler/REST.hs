{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}

-- |
-- Module      : Acessability.Handler.GQL
-- Description : The graphQL API entrypoint
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the handler for graphQL queries
--
module Accessability.Handler.REST (
    getItemR,
    putItemR,
    deleteItemR,    
    postCreateItemR,
    postItemsR) where

--
-- Import standard libs
--
import Data.Text (Text, pack, unpack)
import Data.HexString (HexString(..))
import GHC.Generics (Generic(..))
import Data.Int (Int64)
--
-- Import for morpheus
--
import Data.Morpheus.Kind     (SCALAR, OBJECT, ENUM)
import Data.Morpheus          (interpreter)
import Data.Morpheus.Types    (GQLRootResolver (..),
                              Res,
                              ID(..),
                              MutRes,
                              Undefined(..),
                              GQLType(..),
                              liftEither,
                              GQLRequest(..),
                              GQLResponse(..))
                  
--
-- Yesod and HTTP imports
--
import Yesod
import Network.HTTP.Types (
    status200,
    status400)

--
-- Persist library
--
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql

--
-- My own imports
--
import Accessability.Foundation (Handler, Server(..))
import qualified Accessability.Model.DB as DB
import Accessability.Model
import Accessability.Model.REST
import qualified Accessability.Model.Database as DBF
import Accessability.Model.Transform

-- | The REST get handler, i.e. return with the data of an item based on the items
-- key.
getItemR::Text      -- ^ The item key
    ->Handler Value -- ^ The item as a JSON response
getItemR key = do
    result <- ((toGenericItem <$>) <$>) <$> (DBF.dbFetchItem $ DBF.textToKey key)
    case result of
        Left _ -> sendStatusJSON status400 ()
        Right item -> sendStatusJSON status200 item

-- | The REST delete handler, i.e. return with the data of an item based on the items
-- key and delete the item.
deleteItemR::Text      -- ^ The item key
    ->Handler () -- ^ The item as a JSON response
deleteItemR key = do
    DBF.dbDeleteItem $ DBF.textToKey key
    sendResponseStatus status200 ()

-- | The REST put handler, i.e. return with the updated data of the changed item based
-- on the specified key
putItemR::Text      -- ^ The item key
    ->Handler Value -- ^ The item as a JSON response
putItemR key = do
    queryBody <- requireCheckJsonBody::Handler PutItemBody
    result <- ((toGenericItem <$>) <$>) <$> (DBF.dbUpdateItem (DBF.textToKey key) $
        DBF.changeField DB.ItemName (putItemName queryBody) <>
        DBF.changeField DB.ItemDescription (putItemDescription queryBody) <>
        DBF.changeField DB.ItemLevel (putItemLevel queryBody) <>
        DBF.changeField DB.ItemSource (putItemSource queryBody) <>
        DBF.changeField DB.ItemState (putItemState queryBody) <>
        DBF.changeField DB.ItemLongitude (realToFrac <$> putItemLongitude queryBody) <>
        DBF.changeField DB.ItemLatitude (realToFrac <$> putItemLatitude queryBody))
    case result of
        Left _ -> sendStatusJSON status400 ()
        Right item -> sendStatusJSON status200 item

-- | The REST post handler, i.e. creates a new item with the specified data in the body
-- and return with the data as stored in the database.
postCreateItemR::Handler Value -- ^ The item as a JSON response
postCreateItemR = do
    body <- requireCheckJsonBody::Handler PostItemBody
    result <- ((toGenericItem <$>) <$>) <$> DBF.dbCreateItem $ DB.Item { 
        DB.itemName =  postItemName body,
        DB.itemDescription = postItemDescription body,
        DB.itemLevel = postItemLevel body,
        DB.itemSource = postItemSource body,
        DB.itemState = postItemState body,
        DB.itemLongitude = realToFrac $ postItemLongitude body,
        DB.itemLatitude = realToFrac $ postItemLatitude body }
    case result of
        Left _ -> sendStatusJSON status400 ()
        Right item -> sendStatusJSON status200 item

-- | The REST get handler for items, i.e. a list of items based on a body where the
-- search fields are spceified.
postItemsR::Handler Value    -- ^ The list of items as a JSON response
postItemsR = do
    queryBody <- requireCheckJsonBody::Handler PostItemsBody
    result <- ((toGenericItem <$>) <$>) <$> DBF.dbFetchItems (
        DBF.filter DB.ItemLatitude (<=.) (realToFrac <$> postItemsLatitudeMax queryBody) <>
        DBF.filter DB.ItemLatitude (>=.) (realToFrac <$> postItemsLatitudeMin queryBody) <>
        DBF.filter DB.ItemLongitude (<=.) (realToFrac <$> postItemsLongitudeMax queryBody) <>
        DBF.filter DB.ItemLongitude (>=.) (realToFrac <$> postItemsLongitudeMin queryBody) <>
        ( DBF.filter DB.ItemDescription (DBF.ilike) (postItemsText queryBody) ||.
        DBF.filter DB.ItemName (DBF.ilike) (postItemsText queryBody)) )
        (postItemsLimit queryBody)
    case result of
        Left _ -> sendStatusJSON status400 ()
        Right items -> sendStatusJSON status200 items
