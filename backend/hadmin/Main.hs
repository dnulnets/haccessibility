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
-- This module contains the source to be able to manage the accesibility API
--

module Main where
--
-- Standard libraries
--
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Logger         (runFileLoggingT)
import           Control.Monad.Reader         (ReaderT)

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8        as DB
import qualified Data.ByteString.Lazy         as B
import           Data.Maybe                   (catMaybes)
import qualified Data.Text                    as DT
import qualified Data.Text.Encoding           as DTE
import           Data.Time.Clock              (getCurrentTime)
import           System.Environment           (getArgs, getEnv)

--
-- Persistence libraries
--
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sql

--
-- Get our own items
--
import           Accessability.Data.Geo
import qualified Accessability.Data.Item as ADI
import           Accessability.Model.Database
import           Accessability.Model.REST.Item
import           Accessability.Model.Transform
import           Accessability.Utils.Password
import           Accessability.Data.Functor

--
-- User related functions
--

-- |Adds a user to the database
changePassword::(MonadIO m) => Integer -- ^ bcrypt cost
    ->String                    -- ^ Username
    ->String                    -- ^ Password
    ->ReaderT SqlBackend m ()   -- ^ A database effect
changePassword cost uname upw = do
    pw <- liftIO $ authHashPassword cost $ DT.pack upw
    ukey <- getBy $ UniqueUserUsername $ DT.pack uname
    case ukey of
        (Just (Entity key _)) -> do
            update key [UserPassword =. (DTE.decodeUtf8 pw)]
            liftIO $ putStrLn "User password changed!"
        _ -> do
            liftIO $ putStrLn "User not found!"

-- |Adds a user to the database
addUser::(MonadIO m) => Integer -- ^ bcrypt cost
    ->String                    -- ^ Username
    ->String                    -- ^ Email
    ->String                    -- ^ Password
    ->ReaderT SqlBackend m ()   -- ^ A database effect
addUser cost uname uemail upw = do
    pw <- liftIO $ authHashPassword cost $ DT.pack upw
    ukey <- insert $ User (DT.pack uname) (DTE.decodeUtf8 pw) (DT.pack uemail)
    user <- get ukey
    case user of
        (Just u) -> do
            liftIO $ putStrLn $ DT.unpack $ DTE.decodeUtf8 $ B.toStrict $ encodePretty $ toGenericUser (ukey,u)
        _ -> do
            liftIO $ putStrLn $ "Unable to add user!"

--    liftIO $ printUser user

-- |Parses the JSON file and inserts all items that can be decoded
addItems::(MonadIO m) => String -- ^ Filename
    ->ReaderT SqlBackend m ()   -- ^ A database effect
addItems file = do
    eia <- liftIO (eitherDecodeFileStrict' file::IO (Either String [Maybe PostItemBody]))
    case eia of
        (Left e) -> do
            liftIO $ putStrLn $ "Error encountered during JSON parsing"
            liftIO $ putStrLn $ e
        (Right items) -> do
            forM_ (catMaybes items) storeItem
    where
        storeItem::(MonadIO m) => PostItemBody->ReaderT SqlBackend m ()
        storeItem body = do
            key <- insert Item {
                itemName =  postItemName body,
                itemGuid = postItemGuid body,
                itemCreated = postItemCreated body,
                itemModifier = postItemModifier body,
                itemApproval = postItemApproval body,
                itemDescription = postItemDescription body,
                itemLevel = postItemLevel body,
                itemSource = postItemSource body,
                itemState = postItemState body,
                itemPosition = Position $ PointXY (realToFrac $ postItemLongitude body) (realToFrac $ postItemLatitude body)}
            liftIO $ putStrLn $ "Added item with id " <> (show $ keyToText key)

-- |Adds a user to the database
deleteUser::(MonadIO m) => String -- ^ Username
    ->ReaderT SqlBackend m ()   -- ^ A database effect
deleteUser uname = do
    deleteBy $ UniqueUserUsername (DT.pack $ uname)
    liftIO $ putStrLn $ "User deleted!"

-- |List all users in the database
lsUser::(MonadIO m)=>ReaderT SqlBackend m () -- ^ A database effect
lsUser = do
    users <- selectList [] [Asc UserUsername]
    liftIO $ forM_ users cleanPrint
    where
        cleanPrint::(Entity User)->IO ()
        cleanPrint (Entity key user) = putStrLn $ DT.unpack $ DTE.decodeUtf8 $ B.toStrict $ encodePretty $ toGenericUser (key, user)

-- |Handles the adduser command
handleAddUser:: String  -- ^ The database URL
    ->Integer           -- ^ The bcrypt cost
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleAddUser database cost args = do
    case length args of
        4 -> do
            runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    addUser cost (args !! 1) (args !! 2) (args !! 3)
        _ -> do
            putStrLn "Usage: hadmin adduser <username> <email> <password>"

-- |Handles the chapw command
handleChangePassword:: String  -- ^ The database URL
    ->Integer           -- ^ The bcrypt cost
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleChangePassword database cost args = do
    case length args of
        3 -> do
            runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    changePassword cost (args !! 1) (args !! 2)
        _ -> do
            putStrLn "Usage: hadmin chapw <username> <new password>"

-- |Handles the adduser command
handleDeleteUser:: String  -- ^ The database URL
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleDeleteUser database args = do
    case length args of
        2 -> do
            runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    deleteUser (args !! 1)
        _ -> do
            putStrLn "Usage: hadmin deluser <username>"

-- |Handles the adduser command
handleListUser:: String  -- ^ The database URL
    ->IO ()              -- ^ The effect
handleListUser database = do
    runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
        runSqlPersistMPool lsUser pool

--
-- Items related functions
--

-- |Handles the additems command
handleAddItems:: String  -- ^ The database URL
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleAddItems database args = do
    case length args of
        2 -> do
            runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    addItems (args!!1)
        _ -> do
            putStrLn "Usage: hadmin additems <JSON-file with array of items>"

-- |Handles the delitem command
handleDelItem:: String  -- ^ The database URL
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleDelItem database args = do
    case length args of
        2 -> do
            runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    deleteItem (args!!1)
        _ -> do
            putStrLn "Usage: hadmin delitem <id>"
    where
        -- |Adds a user to the database
        deleteItem::(MonadIO m) => String -- ^ The key
            ->ReaderT SqlBackend m ()     -- ^ A database effect
        deleteItem key = do
            delete $ toKey key
            liftIO $ putStrLn $ "Item deleted!"
            where
                toKey::String->Key Item
                toKey s = textToKey $ DT.pack s

-- |Handles the delitem command
handleListItems:: String  -- ^ The database URL
    ->IO ()             -- ^ The effect
handleListItems database = do
    runFileLoggingT "hadmin.log" $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ do
            listItems
    where
        listItems::(MonadIO m)=>ReaderT SqlBackend m () -- ^ A database effect
        listItems = do
            items <- ffmap clean $ selectList [] [Asc ItemName]
            liftIO $ putStrLn $ DT.unpack $ DTE.decodeUtf8 $ B.toStrict $ encodePretty $ items

        clean::Entity Item->ADI.Item
        clean (Entity k i) = toGenericItem (k,i,Nothing)

-- |The usage information
usage::IO ()
usage = do
    putStrLn "Usage: hadmin <command> [parameters]"
    putStrLn ""
    putStrLn "User commands:"
    putStrLn ""
    putStrLn "adduser   <username> <email> <password>"
    putStrLn "deluser   <username>"
    putStrLn "chapw     <username> <new password>"
    putStrLn "lsusers"
    putStrLn ""
    putStrLn "Item commands:"
    putStrLn ""
    putStrLn "additems  <JSON-file with array of items>"
    putStrLn "delitem   <id>"
    putStrLn "lsitems"
    putStrLn ""
    putStrLn "Format of data:"
    putStrLn ""
    tid <- getCurrentTime
    putStrLn $ "UTC Timestamp: " <> (DT.unpack $ DTE.decodeUtf8 $ B.toStrict $ encode $ tid)
    putStrLn $ "Level: " <> (show [ADI.L1, ADI.L2, ADI.L3, ADI.L4, ADI.L5])
    putStrLn $ "Source: " <> (show [ADI.Human, ADI.Machine])
    putStrLn $ "Modifier: " <> (show [ADI.Static, ADI.Transient])
    putStrLn $ "Approval: " <> (show [ADI.Waiting, ADI.Approved, ADI.Denied])
    putStrLn $ "State: " <> (show [ADI.Unknown, ADI.Online, ADI.Offline])
    putStrLn $ "Id: 0000000000003af5"
    putStrLn ""

-- Example HAPI_DATABASE "postgresql://heatserver:heatserver@yolo.com:5432/heat"
-- Example HAPI_PASSWORD_COST 10

-- | Main starting point for the server
main :: IO ()
main = do
    putStrLn "hadmin 1.0, Written by Tomas Stenlund, Swedish IoT Hub for Accessibility"
    database <- getEnv "HAPI_DATABASE"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"
    args <- getArgs
    if length args > 0
        then case args!!0 of
            "adduser"  -> handleAddUser database cost args
            "lsusers"  -> handleListUser database
            "deluser"  -> handleDeleteUser database args
            "chapw"    -> handleChangePassword database cost args
            "additems" -> handleAddItems database args
            "delitem"  -> handleDelItem database args
            "lsitems"  -> handleListItems database
            _          -> usage
        else
            usage
