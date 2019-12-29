{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Accessability.Foundation
-- Description : The server foundation
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the foundation of the server and defines server and routes
-- types etc. to be able to run the application
--
module Accessability.Foundation (
    Server(..),
    Handler,
    Route (..),
    resourcesServer) where

--
-- Standard libraries
--
import Data.Text (Text, pack)
import Data.Int (Int64)

--
-- Persistence libraries
--
import Database.Persist.Postgresql

--
-- Our own stuff
--
import Accessability.Settings (AppSettings)

--
-- The HTTP server and network libraries
--
import Yesod

-- | Our server and settings
data Server = Server {
    appSettings :: AppSettings             -- ^ Settings for the server
    , serverConnectionPool :: ConnectionPool -- ^ The pool of database connections
}

-- | The routes in our server
mkYesodData "Server" [parseRoutes|
/gql GQLR POST
/api/item CreateItemR POST
/api/item/#Int64 ItemR GET DELETE PUT
/api/items ItemsR POST
/api/authenticate AuthenticateR POST
|]

-- | Our server is a yesod instance
instance Yesod Server

-- | The persistence instance for the server
instance YesodPersist Server where

    -- | The persisten backend
    type YesodPersistBackend Server = SqlBackend

    -- | Executes the database action using the server database pool 
    runDB action = do
        server <- getYesod
        runSqlPool action $ serverConnectionPool server
