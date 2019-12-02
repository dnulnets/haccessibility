{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import Control.Monad (void)
import Control.Exception.Lifted (catch, SomeException(..))

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
--import Yesod.Core
import Yesod.Core.Types (HandlerFor(..))

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
resolveMutation = Mutation { createItem = resolveCreateItem }

-- | The query item resolver
resolveCreateItem ::MutationItemArgs      -- ^ The arguments for the query
                  ->MutRes e Handler Item    -- ^ The result of the query
resolveCreateItem arg =
   liftEither $ dbCreateItem $ Item { itemName =  createItemName arg,
      itemDescription = createItemDescription arg,
      itemLevel = createItemLevel arg,
      itemSource = createItemSource arg,
      itemState = createItemState arg,
      itemPosition = createItemPosition arg
   }

-- | The query resolver
resolveQuery::Query (Res () Handler)
resolveQuery = Query {  queryItem = resolveItem }

-- | The query item resolver
resolveItem::QueryItemArgs          -- ^ The arguments for the query
            ->Res e Handler Item    -- ^ The result of the query
resolveItem QueryItemArgs { queryItemArgsName = arg } =
   liftEither $ dbFetchItem arg   
                                
-- | Fetch the item from the database
dbFetchItem:: Text                           -- ^ The key
        ->Handler (Either String Item)  -- ^ The result of the database search
dbFetchItem name = do
   item <- runDB $ getBy $ DB.UniqueItemName name
   case item of
      Just (Entity itemId item) ->
         return $ Right $ Item { itemName =  DB.itemName item,
                                             itemDescription = DB.itemDescription item,
                                             itemLevel = DB.itemLevel item,
                                             itemSource = DB.itemSource item,
                                             itemState = DB.itemState item,
                                             itemPosition = DB.itemPosition item}
      Nothing ->
         return $ Left "No such item with that name exists"

-- | Creates the item
dbCreateItem:: Item                     -- ^ The key
        ->Handler (Either String Item)  -- ^ The result of the database search
dbCreateItem item = do
   key <- runDB $ insertBy $ DB.Item {DB.itemName = itemName item,
      DB.itemDescription = itemDescription item,
      DB.itemLevel = itemLevel item,
      DB.itemSource = itemSource item,
      DB.itemState = itemState item,
      DB.itemPosition = itemPosition item}
   case key of
      Left (Entity _ dbitem) -> return $ Right $ Item { itemName =  DB.itemName dbitem,
                                    itemDescription = DB.itemDescription dbitem,
                                    itemLevel = DB.itemLevel dbitem,
                                    itemSource = DB.itemSource dbitem,
                                    itemState = DB.itemState dbitem,
                                    itemPosition = DB.itemPosition dbitem}
      Right _ -> return $ Right item
--   return $ Right $ item
--   case key of
--      Just key -> do
--         pitem <- runDB $ get key
--         case pitem of
--            Just dbitem ->
--               return $ Right $ Item { itemName =  DB.itemName dbitem,
--                                             itemDescription = DB.itemDescription dbitem,
--                                             itemLevel = DB.itemLevel dbitem,
--                                             itemSource = DB.itemSource dbitem,
--                                             itemState = DB.itemState dbitem,
--                                             itemPosition = DB.itemPosition dbitem}
--            Nothing ->
--               return $ Left $ "Failed to retrieve information after insert"
--      Nothing ->
--         return $ Left "Failed to insert item, it already exists"

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
