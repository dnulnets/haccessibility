{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Main
-- Description : The main entry point for the accessability grapql server
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the initialization and setup of the graphql API for the
-- accessability database.
--
module Main where

--
-- Standard imports
--
import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

--
-- Monad and applicative stuff
--
import  Control.Applicative        (Applicative)
import  Control.Monad.IO.Class     (MonadIO, liftIO)
import  Control.Monad.Trans.Reader (ReaderT, runReaderT)
import  Control.Monad.Trans.Class  (MonadTrans, lift)
import  Control.Monad.Reader.Class (MonadReader)
import  Control.Monad.Reader       (asks, ask)
import  Control.Monad.Logger       (runStderrLoggingT)
import  Control.Monad.Trans.Resource (runResourceT)

--
-- Yesod, webserver library
--
import  Yesod

--
-- Database library
--
import           Database.Persist.Postgresql as DB

--
-- Import our own stuff
--
import           Accessability.API          (api)
import           Accessability.Model        (migrateAll)
import           Accessability.Server       (Server(..))

--
-- Main starting point
--
main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool  "postgresql://heatserver:heatserver@localhost:30820/heat" 5 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
    warp 3000 $ Server pool
