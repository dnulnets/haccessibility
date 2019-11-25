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
import           Control.Applicative        (Applicative)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Reader       (asks, ask)
import           Control.Monad.Logger       (runStdoutLoggingT)

--
-- Database library
--
import           Database.Persist.Postgresql as DB

--
-- Scotty libraries
--
import Web.Scotty ()
import Web.Scotty.Trans as S

--
-- Import our own stuff
--
import           Accessability.API          (api)
import           Accessability.Model        (migrateAll)
--
-- The Environment for the server
--
data Environment = Environment {
    pool :: ConnectionPool -- ^The connection pool to the database
}

--
-- PostGreSQL connection string
--
connectionString::ConnectionString
connectionString = "postgresql://heatserver:heatserver@localhost:30820/heat"


runDB :: (MonadTrans t, MonadIO (t ApplicationM)) => SqlPersistT IO a -> t ApplicationM a
runDB query = do    
    p <- lift $ asks pool
    liftIO (runSqlPool query p)

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

--
-- The Application Monad
--
newtype ApplicationM a = ApplicationM (ReaderT Environment IO a)
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader Environment)

    --
-- Runs the Application monad, it exposes the IO monad that it contains within
-- the application.
--
runApplicationM::Environment -> ApplicationM a -> IO a
runApplicationM env (ApplicationM r) = runReaderT r env

--
-- The application
--
application :: ApplicationM (ScottyT T.Text ApplicationM ())
application = do
    p <- asks pool
    liftIO (runSqlPool doMigrations p)
    return $ do
        S.get "/" (html "Permobil AB, graphQL prototype accessibility API, version 0.1")        
        S.post "/api" $ raw =<< (liftIO . api =<< body)


--
-- The main
--
main::IO ()
main = do
    p <- runStdoutLoggingT $ createPostgresqlPool connectionString 10
    let env = Environment { pool = p }
    action <- pure $ runApplicationM env
    app <- runApplicationM env application
    scottyT 3000 action app
