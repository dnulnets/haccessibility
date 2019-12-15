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
module Accessability.Handler.GQL (postGQLR) where

--
-- Import standard libs
--
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic(..))

--
-- Import for morpheus
--
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
import Network.HTTP.Types (status200)

--
-- Persist library
--
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql

--
-- My own imports
--
import Accessability.Model.Geo (
    GeodeticPosition(..),
    Geodetic(..),
    WGS84 (..),
    degree,
    meter,
    (*~))
import Accessability.Foundation (Handler, Server(..))
import Accessability.Model.GQL
import qualified Accessability.Model.DB as DB
import Accessability.Model.Transform (
   toGQLItem,
   toDataItem)
import qualified Accessability.Model.Database as DBF

-- | The GraphQL Root resolver
rootResolver :: GQLRootResolver Handler () Query Mutation Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = resolveQuery,
      mutationResolver = resolveMutation,
      subscriptionResolver = Undefined
    }

-- | The mutation resolver
resolveMutation::Mutation (MutRes () Handler)
resolveMutation = Mutation { createItem = resolveCreateItem
                             , deleteItem = resolveDeleteItem
                             , updateItem = resolveUpdateItem }

-- | The mutation create item resolver
resolveUpdateItem ::MutationUpdateItemArgs          -- ^ The arguments for the query
                  ->MutRes e Handler (Maybe Item)   -- ^ The result of the query
resolveUpdateItem arg =
   liftEither $ ((toGQLItem <$>) <$>) <$> (DBF.dbUpdateItem (DBF.idToKey $ updateItemID arg) $
         DBF.changeField DB.ItemName (updateItemName arg) <>
         DBF.changeField DB.ItemDescription (updateItemDescription arg) <>
         DBF.changeField DB.ItemLevel (updateItemLevel arg) <>
         DBF.changeField DB.ItemSource (updateItemSource arg) <>
         DBF.changeField DB.ItemState (updateItemState arg) <>
         DBF.changeField DB.ItemLongitude (realToFrac <$> updateItemLongitude arg) <>
         DBF.changeField DB.ItemLatitude (realToFrac <$> updateItemLatitude arg))
   
-- | The mutation create item resolver
resolveDeleteItem ::MutationDeleteItemArgs   -- ^ The arguments for the query
                  ->MutRes e Handler (Maybe Item)    -- ^ The result of the query
resolveDeleteItem arg = do
   lift $ DBF.dbDeleteItem $ toSqlKey $ read $ unpack $ unpackID $ deleteItemID arg
   return $ Nothing

-- | The mutation create item resolver
resolveCreateItem ::MutationCreateItemArgs   -- ^ The arguments for the query
                  ->MutRes e Handler Item    -- ^ The result of the query
resolveCreateItem arg =
   liftEither $ ((toGQLItem <$>) <$>) <$> DBF.dbCreateItem $ DB.Item { 
      DB.itemName =  createItemName arg,
      DB.itemDescription = createItemDescription arg,
      DB.itemLevel = createItemLevel arg,
      DB.itemSource = createItemSource arg,
      DB.itemState = createItemState arg,
      DB.itemLongitude = realToFrac $ createItemLongitude arg,
      DB.itemLatitude = realToFrac $ createItemLatitude arg
   }

-- | The query resolver
resolveQuery::Query (Res () Handler)
resolveQuery = Query {  queryItem = resolveItem,
                        queryItems = resolveItems }

-- | The query item resolver
resolveItem::QueryItemArgs          -- ^ The arguments for the query
            ->Res e Handler (Maybe Item)    -- ^ The result of the query
resolveItem args = do
   liftEither $ ((toGQLItem <$>) <$>) <$> (DBF.dbFetchItem $ DBF.idToKey $ queryItemID args)
                                
-- | The query item resolver
resolveItems::QueryItemsArgs          -- ^ The arguments for the query
            ->Res e Handler [Item]    -- ^ The result of the query
resolveItems args = do   
   liftEither $ ((toGQLItem <$>) <$>) <$> DBF.dbFetchItems (
      DBF.filter DB.ItemLatitude (<=.) (realToFrac <$> queryItemsLatitudeMax args) <>
      DBF.filter DB.ItemLatitude (>=.) (realToFrac <$> queryItemsLatitudeMin args) <>
      DBF.filter DB.ItemLongitude (<=.) (realToFrac <$> queryItemsLongitudeMax args) <>
      DBF.filter DB.ItemLongitude (>=.) (realToFrac <$> queryItemsLongitudeMin args) <>
      ( DBF.filter DB.ItemDescription (DBF.ilike) (queryItemsText args) ||.
      DBF.filter DB.ItemName (DBF.ilike) (queryItemsText args)) )
      (queryItemsLimit args)

-- | Compose the graphQL api
gqlApi:: GQLRequest         -- ^ The graphql request
   -> Handler GQLResponse   -- ^ The graphql response
gqlApi r = do
    interpreter rootResolver r

-- | The GQL handler
postGQLR::Handler Value -- ^ The graphQL response
postGQLR = do
    request <- requireCheckJsonBody::Handler GQLRequest
    response <- gqlApi request
    sendStatusJSON status200 response
