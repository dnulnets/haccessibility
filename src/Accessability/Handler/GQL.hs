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
import Data.Text (Text, pack)
import GHC.Generics (Generic(..))

--
-- Import for morpheus
--
import Data.Morpheus.Kind     (SCALAR, OBJECT, ENUM)
import Data.Morpheus          (interpreter)
import Data.Morpheus.Types    (GQLRootResolver (..),
                              Res,
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
                             , deleteItem = resolveDeleteItem }

-- | The mutation create item resolver
resolveDeleteItem ::MutationDeleteItemArgs   -- ^ The arguments for the query
                  ->MutRes e Handler (Maybe Item)    -- ^ The result of the query
resolveDeleteItem arg = do
   lift $ dbDeleteItem $ deleteItemName arg
   return $ Nothing

-- | The mutation create item resolver
resolveCreateItem ::MutationCreateItemArgs   -- ^ The arguments for the query
                  ->MutRes e Handler Item    -- ^ The result of the query
resolveCreateItem arg =
   liftEither $ dbCreateItem $ Item { itemName =  createItemName arg,
      itemDescription = createItemDescription arg,
      itemLevel = createItemLevel arg,
      itemSource = createItemSource arg,
      itemState = createItemState arg,
      itemLongitude = createItemLongitude arg,
      itemLatitude = createItemLatitude arg
   }

-- | The query resolver
resolveQuery::Query (Res () Handler)
resolveQuery = Query {  queryItem = resolveItem,
                        queryItems = resolveItems }

-- | The query item resolver
resolveItem::QueryItemArgs          -- ^ The arguments for the query
            ->Res e Handler (Maybe Item)    -- ^ The result of the query
resolveItem QueryItemArgs { queryItemName = arg } =
   liftEither $ dbFetchItem arg   
                                
-- | The query item resolver
resolveItems::QueryItemsArgs          -- ^ The arguments for the query
            ->Res e Handler [Item]    -- ^ The result of the query
resolveItems QueryItemsArgs { queryItemsLatitudeMax = maxLat,
                              queryItemsLongitudeMax = maxLon,
                              queryItemsLongitudeMin = minLon,
                              queryItemsLatitudeMin = minLat } =
   liftEither $ dbFetchItems (realToFrac minLat)
      (realToFrac maxLat)
      (realToFrac minLon)
      (realToFrac maxLon)

-- | Fetch the item from the database
dbFetchItems:: Double->Double              -- ^ Min and max latitude
            -> Double->Double              -- ^ Min and max longitude
            ->Handler (Either String [Item]) -- ^ The result of the database search
dbFetchItems minLat maxLat minLon maxLon = do
   item<- runDB $ selectList [] [Asc DB.ItemName]
   return $ Right $ clean <$> item
   where
      clean (Entity _ dbitem) = toGQLItem dbitem

-- | Fetch the item from the database
dbFetchItem:: Text                           -- ^ The key
        ->Handler (Either String (Maybe Item))  -- ^ The result of the database search
dbFetchItem name = do
   item <- runDB $ getBy $ DB.UniqueItemName name
   case item of
      Just (Entity itemId item) ->
         return $ Right $ Just $ toGQLItem item
      Nothing ->
         return $ Right $ Nothing

-- | Creates the item
dbCreateItem:: Item                     -- ^ The key
        ->Handler (Either String Item)  -- ^ The result of the database search
dbCreateItem item = do
   key <- runDB $ insertBy $ toDataItem item
   case key of
      Left (Entity _ dbitem) -> return $ Right $ toGQLItem dbitem
      Right _ -> return $ Right item

-- | Creates the item
dbDeleteItem:: Text                     -- ^ The key
            ->Handler (Either String ())  -- ^ The result of the database search
dbDeleteItem name = do
   runDB $ deleteBy $ DB.UniqueItemName name
   return $ Right ()

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
