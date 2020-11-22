{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

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
import           Control.Monad                  (forM_)                                                
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( runFileLoggingT )
import           Control.Monad.Reader           ( ReaderT )

import           GHC.Generics                   ( Generic(..) )
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.TH
import qualified Data.ByteString.Char8         as DB
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as DT
import qualified Data.Text.Encoding            as DTE
import           Data.Time.Clock                ( getCurrentTime )
import           Data.HexString                 ( hexString
                                                , toBinary)
import           System.Environment             ( getArgs
                                                , getEnv
                                                )

--
-- Persistence libraries
--
import           Database.Persist
import Database.Persist.Postgresql
    ( SqlBackend, rawSql, runSqlPersistMPool, withPostgresqlPool )
import Database.Persist.Sql ()

--
-- Get our own items
--
import           Accessability.Data.Geo
import qualified Accessability.Data.Item as ADI
import qualified Accessability.Data.User as ADU
import           Accessability.Model.Database
import           Accessability.Model.REST.Item
import           Accessability.Model.Transform
import           Accessability.Utils.Password
import           Accessability.Data.Functor
import           Accessability.Utils.JSON       ( firstLower )
--
--
--

-- | used by set item attributes
data ItemAttribute = ItemAttribute {
    iaName:: DT.Text,
    iaValue:: DT.Text
} deriving (Generic, Show)

-- |Automatically derive JSON but we do not want the first characters in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 2 -- Get rid of the 'ia' in the field names
  } ''ItemAttribute)

--
-- User related functions
--

-- |Adds a user to the database
changePassword
    :: (MonadIO m)
    => Integer -- ^ bcrypt cost
    -> String                    -- ^ Username
    -> String                    -- ^ Password
    -> ReaderT SqlBackend m ()   -- ^ A database effect
changePassword cost uname upw = do
    pw   <- liftIO $ authHashPassword cost $ DT.pack upw
    ukey <- getBy $ UniqueUserUsername $ DT.pack uname
    case ukey of
        (Just (Entity key _)) -> do
            update key [UserPassword =. DTE.decodeUtf8 pw]
            liftIO $ putStrLn "User password changed!"
        _ -> liftIO $ putStrLn "User not found!"

-- |Adds a user to the database
addUser
    :: (MonadIO m)
    => Integer -- ^ bcrypt cost
    -> String                    -- ^ Username
    -> String                    -- ^ Role    
    -> String                    -- ^ Email
    -> String                    -- ^ Password
    -> ReaderT SqlBackend m ()   -- ^ A database effect
addUser cost uname urole uemail upw = do
    pw   <- liftIO $ authHashPassword cost $ DT.pack upw
    ukey <- insert $ User (DT.pack uname) (DTE.decodeUtf8 pw) (DT.pack uemail) (read urole)
    user <- get ukey
    case user of
        (Just u) ->
            liftIO
                $ putStrLn
                $ DT.unpack
                $ DTE.decodeUtf8
                $ B.toStrict
                $ encodePretty
                $ toGenericUser (ukey, u)
        _ -> liftIO $ putStrLn "Unable to add user!"

-- |Parses the JSON file and inserts all items that can be decoded
addItems
    :: (MonadIO m)
    => String -- ^ Filename
    -> ReaderT SqlBackend m ()   -- ^ A database effect
addItems file = do
    eia <-
        liftIO
            (eitherDecodeFileStrict' file :: IO
                  (Either String [Maybe PostItemBody])
            )
    case eia of
        (Left e) -> do
            liftIO $ putStrLn "Error encountered during JSON parsing"
            liftIO $ putStrLn e
        (Right items) -> forM_ (catMaybes items) storeItem
  where
    storeItem :: (MonadIO m) => PostItemBody -> ReaderT SqlBackend m ()
    storeItem body = do
        key <- insert Item
            { itemName        = postItemName body
            , itemGuid        = postItemGuid body
            , itemCreated     = postItemCreated body
            , itemModifier    = postItemModifier body
            , itemApproval    = postItemApproval body
            , itemDescription = postItemDescription body
            , itemSource      = postItemSource body
            , itemPosition    = Position $ PointXY
                                    (realToFrac $ postItemLongitude body)
                                    (realToFrac $ postItemLatitude body)
            }
        liftIO $ putStrLn $ "Added item with id " <> show (keyToText key)

-- |Adds a user to the database
deleteUser
    :: (MonadIO m)
    => String -- ^ Username
    -> ReaderT SqlBackend m ()   -- ^ A database effect
deleteUser uname = do
    deleteBy $ UniqueUserUsername (DT.pack uname)
    liftIO $ putStrLn "User deleted!"

-- |List all users in the database
lsUser :: (MonadIO m) => ReaderT SqlBackend m () -- ^ A database effect
lsUser = do
    users <- selectList [] [Asc UserUsername]
    liftIO $ forM_ users cleanPrint
  where
    cleanPrint :: Entity User -> IO ()
    cleanPrint (Entity key user) =
        putStrLn
            $ DT.unpack
            $ DTE.decodeUtf8
            $ B.toStrict
            $ encodePretty
            $ toGenericUser (key, user)

-- |Handles the adduser command
handleAddUser
    :: String  -- ^ The database URL
    -> Integer           -- ^ The bcrypt cost
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleAddUser database cost args = case length args of
    5 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool -> liftIO $ flip runSqlPersistMPool pool $ addUser
                  cost
                  (args !! 1)
                  (args !! 2)
                  (args !! 3)
                  (args !! 4)
    _ -> putStrLn "Usage: hadmin adduser <username> <role> <email> <password>"

-- |Handles the chapw command
handleChangePassword
    :: String  -- ^ The database URL
    -> Integer           -- ^ The bcrypt cost
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleChangePassword database cost args = case length args of
    3 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool -> liftIO $ flip runSqlPersistMPool pool $ changePassword
                  cost
                  (args !! 1)
                  (args !! 2)
    _ -> putStrLn "Usage: hadmin chapw <username> <new password>"

-- |Handles the adduser command
handleDeleteUser
    :: String  -- ^ The database URL
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleDeleteUser database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool ->
                  liftIO $ flip runSqlPersistMPool pool $ deleteUser (args !! 1)
    _ -> putStrLn "Usage: hadmin deluser <username>"

-- |Handles the list item attributes command
handleListItemAttributes
    :: String  -- ^ The database URL
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleListItemAttributes database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool ->
                  liftIO $ flip runSqlPersistMPool pool $ listItemAttributes
                      (args !! 1)
    _ -> putStrLn "Usage: hadmin lsitemattrs <item id>"
  where

    listItemAttributes :: (MonadIO m) => String -> ReaderT SqlBackend m () -- ^ A database effect
    listItemAttributes key = do
        attributes <- rawSql
            "SELECT ??,?? FROM attribute, attribute_value WHERE attribute.id = attribute_value.attribute AND attribute_value.item=? ORDER BY attribute.name"
            [PersistInt64 (toBinary $ hexString $ DTE.encodeUtf8 (DT.pack key))]
        liftIO
            $ putStrLn
            $ DT.unpack
            $ DTE.decodeUtf8
            $ B.toStrict
            $ encodePretty (cleanup <$> attributes)

    cleanup :: (Entity Attribute, Entity AttributeValue) -> ADI.Attribute
    cleanup (Entity k1 a, Entity k2 v) = ADI.Attribute
        { ADI.attributeDescription      = attributeDescription a
        , ADI.attributeName             = attributeName a
        , ADI.attributeDisplayName      = attributeDisplayName a
        , ADI.attributeGroup            = attributeGroup a
        , ADI.attributeItemId = Just $ keyToText $ attributeValueItem v
        , ADI.attributeTypeof           = attributeTypeof a
        , ADI.attributeUnit             = attributeUnit a
        , ADI.attributeAttributeId      = Just $ keyToText k1
        , ADI.attributeAttributeValueId = Just $ keyToText k2
        , ADI.attributeValue            = Just $ attributeValueValue v
        }

-- |Handles the adduser command
handleListUser
    :: String  -- ^ The database URL
    -> IO ()              -- ^ The effect
handleListUser database =
    runFileLoggingT "hadmin.log"
        $ withPostgresqlPool (DB.pack database) 5
        $ \pool -> liftIO $ runSqlPersistMPool lsUser pool

--
-- Items related functions
--

-- |Handles the additems command
handleAddItems
    :: String  -- ^ The database URL
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleAddItems database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool ->
                  liftIO $ flip runSqlPersistMPool pool $ addItems (args !! 1)
    _ -> putStrLn "Usage: hadmin additems <JSON-file with array of items>"

-- |Handles the delitem command
handleDelItem
    :: String  -- ^ The database URL
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleDelItem database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool ->
                  liftIO $ flip runSqlPersistMPool pool $ deleteItem (args !! 1)
    _ -> putStrLn "Usage: hadmin delitem <id>"
  where
        -- |Adds a user to the database
    deleteItem
        :: (MonadIO m)
        => String -- ^ The key
        -> ReaderT SqlBackend m ()     -- ^ A database effect
    deleteItem key = do
        delete $ toKey key
        liftIO $ putStrLn "Item deleted!"
      where
        toKey :: String -> Key Item
        toKey s = textToKey $ DT.pack s

-- |Handles the delitem command
handleListItems
    :: String  -- ^ The database URL
    -> IO ()             -- ^ The effect
handleListItems database =
    runFileLoggingT "hadmin.log"
        $ withPostgresqlPool (DB.pack database) 5
        $ \pool -> liftIO $ runSqlPersistMPool listItems pool
  where
    listItems :: (MonadIO m) => ReaderT SqlBackend m () -- ^ A database effect
    listItems = do
        items <- ffmap clean $ selectList [] [Asc ItemName]
        liftIO
            $ putStrLn
            $ DT.unpack
            $ DTE.decodeUtf8
            $ B.toStrict
            $ encodePretty items

    clean :: Entity Item -> ADI.Item
    clean (Entity k i) = toGenericItem (k, i, Nothing)

-- |Handles the delitem command
handleListItem
    :: String  -- ^The database URL
    -> [String]  -- ^The key
    -> IO ()             -- ^ The effect
handleListItem database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool -> liftIO $ runSqlPersistMPool (listItems (args !! 1)) pool
    _ -> putStrLn "Usage: hadmin lsitem <id>"

  where
    listItems :: (MonadIO m) => String -> ReaderT SqlBackend m () -- ^ A database effect
    listItems key = do
        items <- ffmap clean
            $ selectList [ItemId ==. textToKey (DT.pack key)] []
        liftIO
            $ putStrLn
            $ DT.unpack
            $ DTE.decodeUtf8
            $ B.toStrict
            $ encodePretty items

    clean :: Entity Item -> ADI.Item
    clean (Entity k i) = toGenericItem (k, i, Nothing)

handleListItemN
    :: String  -- ^The database URL
    -> [String]  -- ^The key
    -> IO ()             -- ^ The effect
handleListItemN database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool -> liftIO $ runSqlPersistMPool (listItems (args !! 1)) pool
    _ -> putStrLn "Usage: hadmin lsitemn <name>"

  where
    listItems :: (MonadIO m) => String -> ReaderT SqlBackend m () -- ^ A database effect
    listItems key = do
        items <- ffmap clean $ selectList [ItemName ==. DT.pack key] []
        liftIO
            $ putStrLn
            $ DT.unpack
            $ DTE.decodeUtf8
            $ B.toStrict
            $ encodePretty items

    clean :: Entity Item -> ADI.Item
    clean (Entity k i) = toGenericItem (k, i, Nothing)

-- |Handles the delitem command
handleListAttributes
    :: String  -- ^ The database URL
    -> IO ()             -- ^ The effect
handleListAttributes database =
    runFileLoggingT "hadmin.log"
        $ withPostgresqlPool (DB.pack database) 5
        $ \pool -> liftIO $ runSqlPersistMPool listAttributes pool
  where
    listAttributes :: (MonadIO m) => ReaderT SqlBackend m () -- ^ A database effect
    listAttributes = do
        items <- ffmap clean $ selectList [] [Asc AttributeName]
        liftIO
            $ putStrLn
            $ DT.unpack
            $ DTE.decodeUtf8
            $ B.toStrict
            $ encodePretty items

    clean :: Entity Attribute -> ADI.Attribute
    clean (Entity k i) = toGenericAttribute (k, i)

--
-- Attributes related functions
--

-- |Parses the JSON file and inserts all items that can be decoded
addAttributes
    :: (MonadIO m)
    => String -- ^ Filename
    -> ReaderT SqlBackend m ()   -- ^ A database effect
addAttributes file = do
    eia <- liftIO
        (eitherDecodeFileStrict' file :: IO
              (Either String [Maybe ADI.Attribute])
        )
    case eia of
        (Left e) -> do
            liftIO $ putStrLn "Error encountered during JSON parsing"
            liftIO $ putStrLn e
        (Right attributes) -> forM_ (catMaybes attributes) storeAttribute
  where
    storeAttribute :: (MonadIO m) => ADI.Attribute -> ReaderT SqlBackend m ()
    storeAttribute body = do
        key <- insert Attribute
            { attributeName        = ADI.attributeName body
            , attributeDisplayName = ADI.attributeDisplayName body
            , attributeGroup       = ADI.attributeGroup body
            , attributeDescription = ADI.attributeDescription body
            , attributeTypeof      = ADI.attributeTypeof body
            , attributeUnit        = ADI.attributeUnit body
            }
        liftIO $ putStrLn $ "Added attribute with id " <> show (keyToText key)

-- |Handles the additems command
handleAddAttributes
    :: String            -- ^ The database URL
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleAddAttributes database args = case length args of
    2 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool -> liftIO $ flip runSqlPersistMPool pool $ addAttributes
                  (args !! 1)
    _ -> putStrLn "Usage: hadmin addattrs <JSON-file with array of attributes>"

-- |Parses the JSON file and inserts all items that can be decoded
addItemAttributes
    :: (MonadIO m)
    => String -- ^The key to the item
    -> String -- ^The file name for the JSON file
    -> ReaderT SqlBackend m ()   -- ^ A database effect
addItemAttributes key file = do
    liftIO $ putStrLn key
    eia <- liftIO
        (eitherDecodeFileStrict' file :: IO
              (Either String [Maybe ItemAttribute])
        )
    case eia of
        (Left e) -> do
            liftIO $ putStrLn "Error encountered during JSON parsing"
            liftIO $ putStrLn e
        (Right attributes) -> do
            deleteWhere [AttributeValueItem ==. textToKey (DT.pack key)]
            forM_ (catMaybes attributes) $ storeAttribute key
  where
    storeAttribute
        :: (MonadIO m)
        => String
        -> ItemAttribute
        -> ReaderT SqlBackend m [Key AttributeValue]
    storeAttribute key body = do
        k <- rawSql
            "insert into attribute_value(attribute, item, value) values ((select id from attribute where name=?),?,?) returning id"
            [ PersistText (iaName body)
            , PersistInt64 (toBinary $ hexString $ DTE.encodeUtf8 (DT.pack key))
            , PersistText (iaValue body)
            ]
        liftIO $ putStrLn $ "Added attribute with id " <> show
            (keyToText (head k))
        pure k

-- |Handles the additems command
handleAddItemAttributes
    :: String            -- ^ The database URL
    -> [String]          -- ^ The command line arguments
    -> IO ()             -- ^ The effect
handleAddItemAttributes database args = case length args of
    3 ->
        runFileLoggingT "hadmin.log"
            $ withPostgresqlPool (DB.pack database) 5
            $ \pool -> liftIO $ flip runSqlPersistMPool pool $ addItemAttributes
                  (args !! 1)
                  (args !! 2)
    _ ->
        putStrLn
            "Usage: hadmin additemattrs <item key> <JSON-file with array of attribute values>"

-- |The usage information
usage :: IO ()
usage = do
    putStrLn ""
    putStrLn "Usage: hadmin <command> [parameters]"
    putStrLn ""
    putStrLn "User commands:"
    putStrLn ""
    putStrLn "adduser   <username> <role> <email> <password>"
    putStrLn "deluser   <username>"
    putStrLn "chapw     <username> <new password>"
    putStrLn "lsusers"
    putStrLn ""
    putStrLn "Item commands:"
    putStrLn ""
    putStrLn "additems  <JSON-file with array of items>"
    putStrLn "delitem   <id>"
    putStrLn "lsitem    <id>"
    putStrLn "lsitemn   <name>"
    putStrLn "lsitems"
    putStrLn ""
    putStrLn "Attribute commands:"
    putStrLn ""
    putStrLn "addattrs     <JSON-file with array of attributes>"
    putStrLn "setitemattrs <id> <JSON-file with array of item attribute values>"
    putStrLn "lsattrs"
    putStrLn "lsitemattrs  <id>"
    putStrLn ""
    putStrLn "Format of data:"
    putStrLn ""
    tid <- getCurrentTime
    putStrLn $ "UTC Timestamp: " <> DT.unpack
        (DTE.decodeUtf8 $ B.toStrict $ encode tid)
    putStrLn $ "Role:" <> show [ADU.Citizen, ADU.Administrator]
    putStrLn $ "Source: " <> show [ADI.Human, ADI.Machine]
    putStrLn $ "Modifier: " <> show [ADI.Static, ADI.Transient]
    putStrLn $ "Approval: " <> show [ADI.Waiting, ADI.Approved, ADI.Denied]
    putStrLn $ "TypeOf: " <> show
        [ADI.BooleanType, ADI.NumberType, ADI.TextType]
    putStrLn ""

-- Example HAPI_DATABASE "postgresql://heatserver:heatserver@yolo.com:5432/heat"
-- Example HAPI_PASSWORD_COST 10

-- | Main starting point for the server
main :: IO ()
main = do
    putStrLn
        "hadmin 1.0, Written by Tomas Stenlund, Swedish IoT Hub for Accessibility"
    database <- getEnv "HAPI_DATABASE"
    cost     <- read <$> getEnv "HAPI_PASSWORD_COST"
    args     <- getArgs
    if not (null args)
        then case head args of
            "adduser"      -> handleAddUser database cost args
            "lsusers"      -> handleListUser database
            "deluser"      -> handleDeleteUser database args
            "chapw"        -> handleChangePassword database cost args
            "additems"     -> handleAddItems database args
            "addattrs"     -> handleAddAttributes database args
            "setitemattrs" -> handleAddItemAttributes database args
            "lsattrs"      -> handleListAttributes database
            "lsitemattrs"  -> handleListItemAttributes database args
            "delitem"      -> handleDelItem database args
            "lsitems"      -> handleListItems database
            "lsitem"       -> handleListItem database args
            "lsitemn"      -> handleListItemN database args
            _              -> usage
        else usage
