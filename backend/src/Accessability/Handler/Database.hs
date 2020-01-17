{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Acessability.Handler.Database
-- Description : The database functions run within the Handler monad
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the functions for handling database queries and conversions to and
-- from database keys.
--
module Accessability.Handler.Database (
    dbFetchItem,
    dbFetchItems,
    dbCreateItem,
    dbDeleteItem,
    dbUpdateItem,
    ilike,
    changeField,
    Accessability.Handler.Database.filter) where

--
-- Import standard libs
--
import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)

--
-- Yesod and HTTP imports
--
import Yesod

--
-- Persist library
--
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Class (ToBackendKey)

--
-- My own imports
--
import Accessability.Foundation (Handler)
import Accessability.Model.Database
import Accessability.Model.Transform (textToKey, keyToText, keyToID, idToKey)

-- | A postgresql backendfilter for ILIKE
ilike::(EntityField Item Text -- ^ The column
   -> Text                       -- ^ The value
   -> Filter Item)            -- ^ The generated filter
ilike field val = Filter field (Left val) (BackendSpecificFilter (pack "ILIKE"))

-- | Create a filter and return as an array so it can be combined
-- easier with other filters
filter::(PersistField a) => EntityField Item a         -- ^ The column
            -> (EntityField Item a -> a -> Filter Item)  -- ^ The operator
            -> Maybe a                                         -- ^ The value
            -> [Filter Item]                                -- ^ The generated filter
filter field operator (Just value) = [operator field value]
filter _ _ Nothing  = []

-- | Create an update filer and return as an array so it can be combined
-- easier with other filters
changeField::(PersistField a) => EntityField Item a -> Maybe a -> [Update Item]
changeField field (Just value) = [field =. value]
changeField _ Nothing  = []

-- | Fetch the item from the database
dbFetchItem :: Key Item                                         -- ^ The key
            ->Handler (Either String (Maybe (Key Item, Item)))  -- ^ The result of the database search
dbFetchItem key = do
   item <- runDB $ get key
   return $ Right $ (key,) <$> item

-- | Fetch the item from the database
dbFetchItems:: [Filter Item]     -- ^ The select item
            -> Maybe Int         -- ^ Max numbr of items
            ->Handler (Either String [(Key Item, Item)]) -- ^ The result of the database search
dbFetchItems filter limit = do
   item <- runDB $ selectList filter [LimitTo $ fromMaybe 10 limit]
   return $ Right $ clean <$> item
   where
      clean (Entity key dbitem) = (key, dbitem)

-- | Creates the item
dbCreateItem:: Item           -- ^ The key
         -> Handler (Either String (Key Item, Item))  -- ^ The result of the database search
dbCreateItem item = do
   key <- runDB $ insertBy item
   case key of
      Left (Entity key dbitem) -> return $ Right (key, dbitem)
      Right key -> return $ Right (key, item)

-- | Delete the item
dbDeleteItem:: Key Item                       -- ^ The key
   ->Handler (Either String ())  -- ^ The result of the database search
dbDeleteItem key = do
   runDB $ delete key
   return $ Right ()

-- | Creates the item
dbUpdateItem:: Key Item       -- ^ The key
            ->[Update Item]   -- ^ Fields to update
            ->Handler (Either String (Maybe (Key Item, Item)))  -- ^ The result of the database update
dbUpdateItem key items = do
   runDB $ update key items
   dbitem <- runDB $ get key
   case dbitem of
      Just dbitem ->
         return $ Right $ Just (key, dbitem)
      Nothing ->
         return $ Right Nothing