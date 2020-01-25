{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Main
-- Description : The main entry point for the functionality to add a user to the
--               accessaibility database.
-- Copyright   : (c) Tomas Stenlund, 2020
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the source to be able to add new users and hash their password
--

module Main where
--
-- Standard libraries
--
import           Control.Monad.Logger               (runStderrLoggingT)
import           Control.Monad.Trans.Resource       (runResourceT)
import           Control.Monad.IO.Class  (liftIO)

import qualified Data.ByteString.Char8              as DB
import qualified Data.Text                          as DT
import qualified Data.Text.Encoding                 as DTE
import           Data.Maybe                         (fromMaybe, listToMaybe)

import           System.Environment                 (getEnv, getArgs)

--
-- Persistence libraries
--
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

--
-- Get our own items
--
import           Accessability.Model.Database
import           Accessability.Utils.Password

--
-- The database migration function
--
mkMigrate "migrateAll" entityDefs

-- Example HAPI_DATABASE "postgresql://heatserver:heatserver@yolo.com:5432/heat"
-- Example HAPI_CERTIFICATE "../deployment/tls.pem"
-- Example HAPI_KEY "../deployment/tls.key"
-- Example HAPI_JWT_SECRET "fwfwefew"
-- Example HAPI_PASSWORD_COST 10
-- Example HAPI_JWT_SESSION_LENGTH 3600

-- | Main starting point for the server
main :: IO ()
main = do
    putStrLn "adduser 1.0, Written by Tomas Stenlund, Swedish IoT Hub for Accessibility\n"
    database <- getEnv "HAPI_DATABASE"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"
    args <- getArgs
    case length args of
        3 -> do
            runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    runMigration migrateAll
                    pw <- liftIO $ authHashPassword cost $ DT.pack (args !!2)
                    u <- insert $ User (DT.pack $ args !! 0) (DT.pack $ (args !! 1)) (DTE.decodeUtf8 pw)                    
                    liftIO $ putStrLn $ "\nUser added:" <> (show u)

        otherwise -> do
            putStrLn "Usage: adduser <username> <email> <password>"