{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
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
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text              (Text)
import GHC.Generics (Generic(..))
import Data.Typeable (Typeable(..))

--
-- Import for morpheus
--

import Data.Morpheus.Kind     (SCALAR, OBJECT, ENUM)
import Data.Morpheus          (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types    (GQLRootResolver (..),
                              IORes,
                              Res,
                              Undefined(..),
                              GQLType(..),
                              liftEitherM,
                              constRes,
                              Res,
                              GQLScalar(..),
                              ScalarValue(..),
                              GQLRequest(..),
                              GQLResponse(..))
                  
--
--
--
import Yesod
import Network.HTTP.Types (
    status200)
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
--import qualified Accessability.Model
import Accessability.Foundation (Handler)
import Accessability.Model.GQL

-- | The GraphQL Root resolver
rootResolver :: GQLRootResolver Handler () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = resolveQuery,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

-- | The query resolver
--resolveQuery::Query ((ExceptT String (HandlerFor s)) ()) -- ^ The result of the query
resolveQuery = Query {  queryItem = resolveItem }

-- | The query item resolver
--resolveItem::QueryItemArgs    -- ^ The arguments for the query
--   ->IORes e Item -- ^ The result of the query
resolveItem QueryItemArgs { queryItemArgsName = arg } =
   liftEitherM $ dbItem arg   
                                
-- | Fetch the item from the database
dbItem :: Text                -- ^ The key
   ->Handler (Either String Item)  -- ^ The result of the database search
dbItem _ = return $ Right $ Item {  itemName =  "NP3 Arena",
                                    itemLevel = L1,
                                    itemPosition = Position Geodetic {
                                      latitude=62.39129 *~ degree, 
                                      longitude=17.3063 *~ degree,
                                      geoAlt=0.0 *~ meter,
                                      ellipsoid=WGS84}
                                   }

-- | Compose the api
api :: GQLRequest    -- ^ The graphql request
   ->Handler GQLResponse  -- ^ The graphql response
api r = do
    interpreter rootResolver r

postGQLR::Handler Value
postGQLR = do
    request <- requireCheckJsonBody::Handler GQLRequest
    response <- api request
    sendStatusJSON status200 response
