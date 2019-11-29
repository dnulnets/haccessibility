{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Server
-- Description : The HTTP server library
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the initialization and setup of the HTTP server that serves both
-- garphql and REST calls.
--
module Accessability.Server (Server(..)) where

--
-- Standard libraries
--
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)

--
-- Persistence libraries
--
import Database.Persist
import Database.Persist.Postgresql

--
-- The HTTP server and network libraries
--
import Yesod
import Network.HTTP.Types (
    status200,
    status201,
    status400,
    status403,
    status404)
  
--
-- The GQL library
--
import Data.Morpheus.Types (GQLRequest(..), GQLResponse(..))

--
-- Our own library
--
import Accessability.API(api)

-- | Our server
data Server = Server ConnectionPool

-- | The routes in our server
mkYesod "Server" [parseRoutes|
/gql GQL POST
|]

-- | Our server is a yesod instance
instance Yesod Server

-- | The persistence instance for the server
instance YesodPersist Server where

    -- | The persisten backend
    type YesodPersistBackend Server = SqlBackend

    -- | Executes the database action using the server database pool 
    runDB action = do
        Server pool <- getYesod
        runSqlPool action pool

-- | Handle the GraphQL request that comes in the body of the post by executing it and
-- returning with the response
postGQL :: Handler Value -- ^ The JSON resonse from the GQL request
postGQL = do
    request <- requireCheckJsonBody::Handler GQLRequest
    response <- liftIO $ api request
    sendStatusJSON status201 response
