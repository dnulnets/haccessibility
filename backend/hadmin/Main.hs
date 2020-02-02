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
import           Control.Monad (forM_)
import           Control.Monad.Logger               (runStderrLoggingT)
import           Control.Monad.Trans.Resource       (runResourceT)
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Control.Monad.Reader (ReaderT)

import qualified Data.ByteString.Char8              as DB
import qualified Data.Text                          as DT
import qualified Data.Text.Encoding                 as DTE
import qualified Data.ByteString.Lazy as B
import           Data.Maybe                         (fromMaybe, listToMaybe, catMaybes)
import           Data.Aeson                         (eitherDecodeFileStrict')
import           Data.Int
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Aeson
import           System.Environment                 (getEnv, getArgs)

--
-- Persistence libraries
--
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql
import           Database.Persist.TH

--
-- Get our own items
--
import           Accessability.Model.Database
import           Accessability.Utils.Password
import           Accessability.Model.REST
import           Accessability.Data.Geo

-- |Print user information to stdout
printUser::Maybe User   -- ^ The user to be printed
    -> IO ()            -- ^ IO effect
printUser Nothing = do
    putStrLn "No user"
printUser (Just u) = do
    putStrLn $ "Username: " <> (DT.unpack $ userUsername u)
    putStrLn $ "Email: " <> (DT.unpack $ userEmail u)

-- |Print user information to stdout
printItem::Entity Item   -- ^ The item to be printed
    -> IO ()            -- ^ IO effect
printItem (Entity key u) = do
    putStrLn $ "Key: " <> (show $ fromSqlKey key)
    putStrLn $ "Name: " <> (DT.unpack $ itemName u)
    putStrLn $ "Description: " <> (DT.unpack $ itemDescription u)
    putStrLn $ "Position: " <> (show $ itemPosition u)
    putStrLn $ "Level: " <> (show $ itemLevel u)
    putStrLn $ "Source: " <> (show $ itemSource u)
    putStrLn $ "State: " <> (show $ itemState u)
    putStrLn $ "Modifier: " <> (show $ itemModifier u)
    putStrLn $ "Approval: " <> (show $ itemApproval u)
    putStrLn $ "Created: " <> (DT.unpack $ DTE.decodeUtf8 $ B.toStrict $ encode $ itemCreated u)
    putStrLn ""

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
            liftIO $ putStrLn "\nUser password changed!"
        otherwise -> do
            liftIO $ putStrLn "\nUser not found!"

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
    liftIO $ putStrLn $ "\nUser added!"
    liftIO $ printUser user

-- |Parses the JSON file and inserts all items that can be decoded
addItems::(MonadIO m) => String -- ^ Filename
    ->ReaderT SqlBackend m ()   -- ^ A database effect
addItems file = do
    eia <- liftIO (eitherDecodeFileStrict' file::IO (Either String [Maybe PostItemBody]))
    case eia of
        (Left error) -> do
            liftIO $ putStrLn $ "Error encountered during JSON parsing"
            liftIO $ putStrLn $ error
        (Right items) -> do
            forM_ (catMaybes items) storeItem
            liftIO $ putStrLn "Items added!"
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
            liftIO $ putStrLn $ "Added item with key=" <> (show $ fromSqlKey key)

-- |Adds a user to the database
deleteUser::(MonadIO m) => String -- ^ Username
    ->ReaderT SqlBackend m ()   -- ^ A database effect
deleteUser uname = do
    deleteBy $ UniqueUserUsername (DT.pack $ uname)
    liftIO $ putStrLn $ "\nUser deleted!"

-- |List all users in the database
lsUser::(MonadIO m)=>ReaderT SqlBackend m () -- ^ A database effect
lsUser = do
    users <- selectList [] [Asc UserUsername]
    liftIO $ putStrLn "\nList of users\n"
    liftIO $ forM_ (clean <$> users) printUser
    where
        clean::(Entity User)->Maybe User
        clean (Entity _ user) = Just user

-- |Handles the adduser command
handleAddUser:: String  -- ^ The database URL
    ->Integer           -- ^ The bcrypt cost
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleAddUser database cost args = do
    case length args of
        4 -> do
            runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    addUser cost (args !! 1) (args !! 2) (args !! 3)
        otherwise -> do
            putStrLn "Usage: hadmin adduser <username> <email> <password>"

-- |Handles the chapw command
handleChangePassword:: String  -- ^ The database URL
    ->Integer           -- ^ The bcrypt cost
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleChangePassword database cost args = do
    case length args of
        3 -> do
            runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    changePassword cost (args !! 1) (args !! 2)
        otherwise -> do
            putStrLn "Usage: hadmin chapw <username> <new password>"

-- |Handles the adduser command
handleDeleteUser:: String  -- ^ The database URL
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleDeleteUser database args = do
    case length args of
        2 -> do
            runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    deleteUser (args !! 1)
        otherwise -> do
            putStrLn "Usage: hadmin deluser <username>"

-- |Handles the adduser command
handleListUser:: String  -- ^ The database URL
    ->IO ()              -- ^ The effect
handleListUser database = do
    runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
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
            runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    addItems (args!!1)
        otherwise -> do
            putStrLn "Usage: hadmin additems <JSON-file with array of items>"

-- |Handles the delitem command
handleDelItem:: String  -- ^ The database URL
    ->[String]          -- ^ The command line arguments
    ->IO ()             -- ^ The effect
handleDelItem database args = do
    case length args of
        2 -> do
            runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
                flip runSqlPersistMPool pool $ do
                    deleteItem (args!!1)
        otherwise -> do
            putStrLn "Usage: hadmin delitem <key>"
    where
        -- |Adds a user to the database
        deleteItem::(MonadIO m) => String -- ^ The key
            ->ReaderT SqlBackend m ()     -- ^ A database effect
        deleteItem key = do
            delete $ toKey key
            liftIO $ putStrLn $ "\nItem deleted!"
            where
                toKey::String->Key Item
                toKey s = toSqlKey (read s) 

-- |Handles the delitem command
handleListItems:: String  -- ^ The database URL
    ->IO ()             -- ^ The effect
handleListItems database = do
    runStderrLoggingT $ withPostgresqlPool (DB.pack database) 5 $ \pool -> liftIO $ do
        flip runSqlPersistMPool pool $ do
            listItems
    where
        listItems::(MonadIO m)=>ReaderT SqlBackend m () -- ^ A database effect
        listItems = do
            items <- selectList [] [Asc ItemName]
            liftIO $ putStrLn "\nList of items:"
            liftIO $ forM_ items printItem

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
    putStrLn "lsitems"
    putStrLn ""
    putStrLn "Item commands:"
    putStrLn ""
    putStrLn "additems  <JSON-file with array of items>"
    putStrLn "delitem   <key>"
    putStrLn "lsitems"
    putStrLn ""
    tid <- getCurrentTime
    putStrLn $ DT.unpack $ DTE.decodeUtf8 $ B.toStrict $ encode $ tid

-- Example HAPI_DATABASE "postgresql://heatserver:heatserver@yolo.com:5432/heat"
-- Example HAPI_PASSWORD_COST 10

-- | Main starting point for the server
main :: IO ()
main = do
    putStrLn "hadmin 1.0, Written by Tomas Stenlund, Swedish IoT Hub for Accessibility\n"
    database <- getEnv "HAPI_DATABASE"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"
    args <- getArgs
    if length args > 0
        then case args!!0 of
            "adduser" -> handleAddUser database cost args
            "lsusers" -> handleListUser database
            "deluser" -> handleDeleteUser database args
            "chapw" -> handleChangePassword database cost args
            "additems" -> handleAddItems database args
            "delitem" -> handleDelItem database args
            "lsitems" -> handleListItems database
            otherwise -> usage
        else
            usage
