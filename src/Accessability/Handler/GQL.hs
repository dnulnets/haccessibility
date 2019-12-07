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
import qualified Accessability.Model.Data as DB
import Accessability.Model.Transform (toGQLItem, toDataItem)
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
   liftEither $ ((toGQLItem <$>) <$>) <$> (DBF.dbUpdateItem (DBF.theKey $ updateItemID arg) $
         changeField DB.ItemName (updateItemName arg) <>
         changeField DB.ItemDescription (updateItemDescription arg) <>
         changeField DB.ItemLevel (updateItemLevel arg) <>
         changeField DB.ItemSource (updateItemSource arg) <>
         changeField DB.ItemState (updateItemState arg) <>
         changeField DB.ItemLongitude (realToFrac <$> updateItemLongitude arg) <>
         changeField DB.ItemLatitude (realToFrac <$> updateItemLatitude arg))

   where
   
      changeField::(PersistField a) => EntityField DB.Item a -> Maybe a -> [Update DB.Item]
      changeField field (Just value) = [field =. value]
      changeField _ Nothing  = []
   
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
   liftEither $ ((toGQLItem <$>) <$>) <$> (DBF.dbFetchItem $ DBF.theKey $ queryItemID args)
                                
-- | The query item resolver
resolveItems::QueryItemsArgs          -- ^ The arguments for the query
            ->Res e Handler [Item]    -- ^ The result of the query
resolveItems args = do   
   liftEither $ ((toGQLItem <$>) <$>) <$> DBF.dbFetchItems (
      filterField DB.ItemLatitude (<=.) (realToFrac <$> queryItemsLatitudeMax args) <>
      filterField DB.ItemLatitude (>=.) (realToFrac <$> queryItemsLatitudeMin args) <>
      filterField DB.ItemLongitude (<=.) (realToFrac <$> queryItemsLongitudeMax args) <>
      filterField DB.ItemLongitude (>=.) (realToFrac <$> queryItemsLongitudeMin args))
      (queryItemsLimit args)

   where

      filterField::(PersistField a) => EntityField DB.Item a
                  -> (EntityField DB.Item a -> a -> Filter DB.Item)
                  -> Maybe a
                  -> [Filter DB.Item]
      filterField field operator (Just value) = [operator field value]
      filterField _ _ Nothing  = []
   
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
