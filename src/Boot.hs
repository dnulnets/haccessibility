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
    
import Accessability.Model.DB (
    migrateAll)

--
-- The dispatcher
--
mkYesodDispatch "Server" resourcesServer

-- | Main starting point for the server
serverMain :: IO ()
serverMain = do
    database <- (maybe "haccdb:5432" id) . listToMaybe <$> getArgs
    runStderrLoggingT $ withPostgresqlPool (pack ("postgresql://heatserver:heatserver@" <> database <> "/heat")) 5 $ \pool -> liftIO $ do
        runResourceT $ flip runSqlPool pool $ do
            runMigration migrateAll
        warp 3000 $ Server { serverConnectionPool = pool }
