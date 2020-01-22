{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}

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
import           Control.Monad.Logger               (runStderrLoggingT)
import           Control.Monad.Trans.Resource       (runResourceT)
import qualified Data.ByteString.Char8              as DB
import qualified Data.Text                          as DT
import           Data.Maybe                         (fromMaybe, listToMaybe)

import           System.Environment                 (getEnv)
import           System.Random
--
-- Persistence libraries
--
import           Database.Persist.Postgresql

--
-- The HTTP server and network libraries
--
import qualified Network.Wai.Handler.Warp           as WAI
import qualified Network.Wai.Handler.WarpTLS        as WAIT
import           Yesod
import           Yesod.Static
--
-- Get our own items
--
import           Accessability.Foundation           (Route (..), Server (..),
                                                     resourcesServer)

import           Accessability.Handler.GQL          (postGQLR)

import           Accessability.Handler.REST         (deleteItemR, getItemR,
                                                     postCreateItemR,
                                                     postItemsR, putItemR)

import           Accessability.Handler.Authenticate (postAuthenticateR)

import           Accessability.Settings             (defaultSettings, AppSettings(..))

import           Accessability.Middleware           (corsified)

import           Accessability.Model.Database       (entityDefs)

--
-- The dispatcher
--
mkYesodDispatch "Server" resourcesServer

--
-- The database migration function
--
mkMigrate "migrateAll" entityDefs

-- Example HAPI_DATABASE "postgresql://heatserver:heatserver@yolo.com:5432/heat"
-- Example HAPI_CERTIFICATE "../deployment/tls.pem"
-- Example HAPI_KEY "../deployment/tls.key"
-- Example HAPI_JWT_SECRET "mandelmassa"
-- Example HAPI_JWT_COST 

-- | Main starting point for the server
serverMain :: IO ()
serverMain = do
    gen <- newStdGen 
    let jwtSecret = take 10 $ randomRs ('a','z') gen 
    database <- getEnv "HAPI_DATABASE"
    pem <- getEnv "HAPI_CERTIFICATE"
    key <- getEnv "HAPI_KEY"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"    
    time <- read <$> getEnv "HAPI_JWT_SESSION_LENGTH"
    mystatic <- static "static"
    runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
        runResourceT $ flip runSqlPool pool $
            runMigration migrateAll
        application <- toWaiApp $ Server {
            getStatic = mystatic,
            appSettings = defaultSettings {
                tokenSecret = DT.pack jwtSecret,
                passwordCost = cost,
                tokenExpiration = time},
            serverConnectionPool = pool }
        WAIT.runTLS (WAIT.tlsSettings pem key)
            (WAI.setServerName "Accessability Server - IoTHub Sweden"
            (WAI.setHost "*" WAI.defaultSettings)) $ corsified application
