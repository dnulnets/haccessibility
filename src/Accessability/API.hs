{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE NamedFieldPuns       #-}

-- |
-- Module      : Acessability.API
-- Description : The graphQL API entrypoint
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the main entry point for the graphQL interface to the
-- accessability database.
--
module Accessability.API (api) where

--
-- Import standard libs
--
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text              (Text)

--
-- Import for morpheus
--
import Data.Morpheus.Kind     (SCALAR)
import Data.Morpheus          (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types    (GQLRootResolver (..),
                              IORes, 
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
-- My own imports
--
import Accessability.Geo      (GeodeticPosition(..),
                              Geodetic(..),
                              WGS84 (..),
                              degree,
                              meter,
                              (*~))

--
-- Import the schema
--
$(importGQLDocumentWithNamespace "schema/accessibility.gql")

--
-- Create the root resolver
--
rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    {
      queryResolver = resolveQuery,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

resolveQuery::Query (IORes ())
resolveQuery = Query {  queryItem = resolveItem,
                        queryAllItems = resolveAllItems }

--
-- Resolver
--
resolveItem::QueryItemArgs->IORes e (Item (IORes e))
resolveItem QueryItemArgs { queryItemArgsName } =
   liftEitherM $ dbItem queryItemArgsName

resolveAllItems::()->IORes e (Item (IORes e))
resolveAllItems _ =
   liftEitherM $ dbItem $ ""

--
-- Database fetching
--
dbItem :: Text->IO (Either String (Item (IORes e)))
dbItem _ = return $ Right $ Item {  itemName =  constRes $ "NP3 Arena",
                                    itemLevel = constRes L1,
                                    itemPosition = constRes $ Geodetic {
                                      latitude=62.39129 *~ degree, 
                                      longitude=17.3063 *~ degree,
                                      geoAlt=0.0 *~ meter,
                                      ellipsoid=WGS84}
                                   }

--
-- The exported api
--
api :: GQLRequest->IO GQLResponse
api a = do
--    putStrLn $ show a
    out <- interpreter rootResolver a
    -- putStrLn $ B.unpack out
    return out
