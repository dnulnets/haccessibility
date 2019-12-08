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
module Accessability.Handler.REST (getItemR, postItemsR) where

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
import Accessability.Model.Generic
import Accessability.Model.REST
import qualified Accessability.Model.Database as DBF
import Accessability.Model.Transform

-- | The REST get handler
getItemR::Text      -- ^ The item key
    ->Handler Value    -- ^ The item as a JSON response
getItemR key = do
    result <- ((toGenericItem <$>) <$>) <$> (DBF.dbFetchItem $ DBF.textToKey key)
    case result of
        Left _ -> sendStatusJSON status400 ()
        Right item -> sendStatusJSON status200 item

-- | The REST get handler
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
