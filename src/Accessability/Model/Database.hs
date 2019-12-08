{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}

-- |
-- Module      : Acessability.Model.Database
-- Description : The database functions
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the functions for handling database queries
--
module Accessability.Model.Database (
    dbFetchItem,
    dbFetchItems,
    dbCreateItem,
    dbDeleteItem,
    dbUpdateItem,
    idToKey,
    textToKey,
    ilike,
    Accessability.Model.Database.filter) where

--
-- Import standard libs
--
import Data.Text (Text, pack, unpack)
                  
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

import Data.Morpheus.Types (ID(..))

--
-- My own imports
--
import Accessability.Foundation (Handler)
import Accessability.Model.DB

-- | Convert from ID to database key
idToKey::ID -> Key Item
idToKey key = toSqlKey $ read $ unpack $ unpackID $ key

-- | Convert from ID to database key
textToKey::Text -> Key Item
textToKey key = toSqlKey $ read $ unpack $ key

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

-- | Fetch the item from the database
dbFetchItem :: Key Item                                         -- ^ The key
            ->Handler (Either String (Maybe (Key Item, Item)))  -- ^ The result of the database search
dbFetchItem key = do
   item <- runDB $ get key
   return $ Right $ (\i->(key,i)) <$> item

-- | Fetch the item from the database
dbFetchItems:: [Filter Item]     -- ^ The select item
            -> Maybe Int         -- ^ Max numbr of items
            ->Handler (Either String [(Key Item, Item)]) -- ^ The result of the database search
dbFetchItems filter limit = do
   item <- runDB $ selectList filter [LimitTo $ maybe 10 id limit]
   return $ Right $ clean <$> item
   where
      clean (Entity key dbitem) = (key, dbitem)

-- | Creates the item
dbCreateItem:: Item           -- ^ The key
         -> Handler (Either String (Key Item, Item))  -- ^ The result of the database search
dbCreateItem item = do
   key <- runDB $ insertBy $ item
   case key of
      Left (Entity key dbitem) -> return $ Right $ (key, dbitem)
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
         return $ Right $ Just $ (key, dbitem)
      Nothing ->
         return $ Right $ Nothing