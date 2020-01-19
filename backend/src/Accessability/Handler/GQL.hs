{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

-- |
-- Module      : Acessability.Handler.GQL
-- Description : The graphQL API entrypoint
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the handler for graphQL queries route
--
module Accessability.Handler.GQL (postGQLR) where

--
-- Import standard libs
--
import Data.Text (pack, unpack, splitOn)
import qualified UnliftIO.Exception as UIOE

--
-- Import for morpheus
--
import Data.Morpheus          (interpreter)
import Data.Morpheus.Types    (GQLRootResolver (..),
                              Res,
                              ID(..),
                              MutRes,
                              Undefined(..),
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
import Database.Persist.Sql

--
-- My own imports
--
import Accessability.Data.Functor
import Accessability.Data.Geo
import Accessability.Foundation (Handler, requireAuthentication)
import Accessability.Model.GQL
import qualified Accessability.Model.Database as DB
import Accessability.Model.Transform (
   toGQLItem,
   idToKey)
import qualified Accessability.Handler.Database as DBF

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
   fffmap toGQLItem liftEither $ DBF.dbUpdateItem (idToKey $ updateItemId arg) $
         DBF.changeField DB.ItemName (updateItemName arg) <>
         DBF.changeField DB.ItemDescription (updateItemDescription arg) <>
         DBF.changeField DB.ItemLevel (updateItemLevel arg) <>
         DBF.changeField DB.ItemSource (updateItemSource arg) <>
         DBF.changeField DB.ItemState (updateItemState arg) <>
         DBF.changeField DB.ItemPosition (position (realToFrac <$> updateItemLongitude arg) (realToFrac <$> updateItemLatitude arg))

-- | The mutation create item resolver
resolveDeleteItem ::MutationDeleteItemArgs   -- ^ The arguments for the query
                  ->MutRes e Handler (Maybe Item)    -- ^ The result of the query
resolveDeleteItem arg = do
   _ <- lift $ DBF.dbDeleteItem $ toSqlKey $ read $ unpack $ unpackID $ deleteItemId arg
   return Nothing

-- | The mutation create item resolver
resolveCreateItem ::MutationCreateItemArgs   -- ^ The arguments for the query
                  ->MutRes e Handler Item    -- ^ The result of the query
resolveCreateItem arg =
   ffmap toGQLItem liftEither $ DBF.dbCreateItem $ DB.Item {
      DB.itemName =  createItemName arg,
      DB.itemDescription = createItemDescription arg,
      DB.itemLevel = createItemLevel arg,
      DB.itemSource = createItemSource arg,
      DB.itemState = createItemState arg,
      DB.itemPosition = Position $ PointXY (realToFrac $ createItemLongitude arg) (realToFrac $ createItemLatitude arg)}

-- | The query resolver
resolveQuery::Query (Res () Handler)
resolveQuery = Query {  queryItem = resolveItem,
                        queryItems = resolveItems }

-- | The query item resolver
resolveItem::QueryItemArgs          -- ^ The arguments for the query
            ->Res e Handler (Maybe Item)    -- ^ The result of the query
resolveItem args =
   fffmap toGQLItem liftEither $ DBF.dbFetchItem (idToKey $ queryItemId args)

-- | The query item resolver
resolveItems::QueryItemsArgs          -- ^ The arguments for the query
            ->Res e Handler [Item]    -- ^ The result of the query
resolveItems args =
   fffmap toGQLItem liftEither $ DBF.dbFetchItems (
--      DBF.filter DB.ItemLatitude (<=.) (realToFrac <$> queryItemsLatitudeMax args) <>
--      DBF.filter DB.ItemLatitude (>=.) (realToFrac <$> queryItemsLatitudeMin args) <>
--      DBF.filter DB.ItemLongitude (<=.) (realToFrac <$> queryItemsLongitudeMax args) <>
--      DBF.filter DB.ItemLongitude (>=.) (realToFrac <$> queryItemsLongitudeMin args) <>
      ( DBF.filter DB.ItemDescription DBF.ilike (queryItemsText args) ||.
      DBF.filter DB.ItemName DBF.ilike (queryItemsText args)) )
      (queryItemsLimit args)

-- | Compose the graphQL api
gqlApi:: GQLRequest         -- ^ The graphql request
   -> Handler GQLResponse   -- ^ The graphql response
gqlApi = interpreter rootResolver

-- | The GQL handler
postGQLR::Handler Value -- ^ The graphQL response
postGQLR = do
   requireAuthentication
   request <- requireCheckJsonBody::Handler GQLRequest
   response <- UIOE.catchAny (Right <$> gqlApi request) (pure . Left . show)
   case response of
      Left e -> invalidArgs $ ["Unable to find any items in the database"] <> splitOn "\n" (pack e)
      Right r -> sendStatusJSON status200 r