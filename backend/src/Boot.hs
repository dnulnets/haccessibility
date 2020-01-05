{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Boot
-- Description : The bootstrap for the HTTP server
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the initialization and setup of the server that serves both
-- HTTP graphql and rest calls.
--
module Boot (serverMain) where

--
-- Standard libraries
--
import System.Environment (getArgs)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (listToMaybe, maybe)
import Data.ByteString.Char8 (pack)

--
-- Persistence libraries
--
import Database.Persist.Postgresql

--
-- The HTTP server and network libraries
--
import Yesod
import Yesod.Static
import qualified Network.Wai.Handler.Warp as WAI
import qualified Network.Wai.Handler.WarpTLS as WAIT
--
-- Get our own items
--
import Accessability.Foundation (
    Handler,
    Server(..),
    Route(..),
    resourcesServer)

import Accessability.Handler.GQL (
    postGQLR)

import Accessability.Handler.REST (
    getItemR,
    putItemR,
    deleteItemR,
    postCreateItemR,
    postItemsR)

import Accessability.Handler.Authenticate (
    postAuthenticateR)

import Accessability.Settings (defaultSettings)

import Accessability.Middleware (corsified)

import Accessability.Model.DB (entityDefs)

--
-- The dispatcher
--
mkYesodDispatch "Server" resourcesServer

--
-- The database migration function
--
mkMigrate "migrateAll" entityDefs

-- | Main starting point for the server
serverMain :: IO ()
serverMain = do
    database <- (maybe "haccdb:5432" id) . listToMaybe <$> getArgs
    static@(Static settings) <- static "static"
    runStderrLoggingT $ withPostgresqlPool (pack ("postgresql://heatserver:heatserver@" <> database <> "/heat")) 5 $ \pool -> liftIO $ do
        runResourceT $ flip runSqlPool pool $ do
            runMigration migrateAll
        application <- toWaiApp $ Server { getStatic = static, 
            appSettings = defaultSettings,
            serverConnectionPool = pool }
        WAIT.runTLS (WAIT.tlsSettings "../deployment/tls.pem" "../deployment/tls.key") 
            (WAI.setServerName "Accessability Server - IoTHub Sweden" 
            (WAI.setHost "*" WAI.defaultSettings)) $ corsified application
