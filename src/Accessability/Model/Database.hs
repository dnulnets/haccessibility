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
    theKey) where

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
import Accessability.Model.Data

-- | Convert from ID to database key
theKey::ID -> Key Item
theKey key = toSqlKey $ read $ unpack $ unpackID $ key

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