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
import Accessability.Foundation (Handler)
import Accessability.Model.GQL

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
resolveCreateItem MutationItemArgs { mutationItemArgsName = arg } =
   liftEither $ dbItem arg

-- | The query resolver
resolveQuery::Query (Res () Handler)
resolveQuery = Query {  queryItem = resolveItem }

-- | The query item resolver
resolveItem::QueryItemArgs          -- ^ The arguments for the query
            ->Res e Handler Item    -- ^ The result of the query
resolveItem QueryItemArgs { queryItemArgsName = arg } =
   liftEither $ dbItem arg   
                                
-- | Fetch the item from the database
dbItem:: Text                           -- ^ The key
        ->Handler (Either String Item)  -- ^ The result of the database search
dbItem _ = return $ Right $ Item {  itemName =  pack "NP3 Arena",
                                    itemLevel = L1,
                                    itemPosition = Position Geodetic {
                                      latitude=62.39129 *~ degree, 
                                      longitude=17.3063 *~ degree,
                                      geoAlt=0.0 *~ meter,
                                      ellipsoid=WGS84}
                                   }

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
