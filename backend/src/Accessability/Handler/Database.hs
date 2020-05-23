{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
    dbFetchAttributes,
    dbFetchItemAttributes,
    dbUpdateItemAttributes,
    ilike,
    changeField,
    Accessability.Handler.Database.filter) where

--
-- Import standard libs
--
import           Data.Text                    (Text, pack)
import           Control.Monad (void)
import           Control.Monad.Reader           ( ReaderT )
--
--
--
import           Database.Persist
import           Database.Persist.Sql

--
-- Yesod and HTTP imports
--
import           Yesod

--
-- My own imports
--
import           Accessability.Data.Geo
import           Accessability.Foundation     (Handler)
import           Accessability.Model.Database

-- | A postgresql backendfilter for ILIKE
ilike::(EntityField Item Text -- ^ The column
   -> Text                    -- ^ The value
   -> Filter Item)            -- ^ The generated filter
ilike field val = Filter field (Left val) (BackendSpecificFilter (pack "ILIKE"))

-- | Create a filter and return it as an array so it can be combined
-- easier with other filters
filter::(PersistField a) => EntityField Item a           -- ^ The column
            -> (EntityField Item a -> a -> Filter Item)  -- ^ The operator
            -> Maybe a                                   -- ^ The value
            -> [Filter Item]                             -- ^ The generated filter
filter field operator (Just value) = [operator field value]
filter _ _ Nothing                 = []

-- | Create an update filer and return it as an array so it can be combined
-- easier with other filters
changeField::(PersistField a) => EntityField Item a -- ^ The coumn
   -> Maybe a                                       -- ^ The value
   -> [Update Item]                                 -- ^ The update filter
changeField field (Just value) = [field =. value]
changeField _ Nothing          = []

-- | Fetch all possible attributes from the database within the Handler monad
dbFetchAttributes :: Handler (Either String [(Key Attribute, Attribute)])  -- ^ The result of the database search
dbFetchAttributes = do
   attributes <- runDB $ selectList [] [Asc AttributeName]
   return $ Right $ cleanup <$> attributes
   where
      cleanup::Entity Attribute->(Key Attribute, Attribute)
      cleanup (Entity k a) = (k, a)

-- | Fetch the items attributes from the database within the Handler monad
dbFetchItemAttributes :: Key Item->Handler (Either String [(Key Attribute, Attribute, Key AttributeValue, AttributeValue)])  -- ^ The result of the database search
dbFetchItemAttributes key = do
   attributes <- runDB $ rawSql "SELECT ??,?? FROM attribute, attribute_value WHERE attribute.id = attribute_value.attribute AND attribute_value.item=? ORDER BY attribute.name" [PersistInt64 (fromSqlKey key)]
   return $ Right $ cleanup <$> attributes
   where
      cleanup::(Entity Attribute,Entity AttributeValue)->(Key Attribute, Attribute, Key AttributeValue, AttributeValue)
      cleanup (Entity k1 a, Entity k2 v) = (k1, a, k2, v)

-- | Fetch the item from the database within the Handler monad
dbFetchItem :: Key Item                                                       -- ^ The key
            ->Handler (Either String (Maybe (Key Item, Item, Maybe Double)))  -- ^ The result of the database search
dbFetchItem key = do
   item <- runDB $ get key
   return $ Right $ cleanup key <$> item
   where
      cleanup::Key Item->Item->(Key Item, Item, Maybe Double)
      cleanup k i = (k, i, Nothing)

-- | Fetch all items from the database that fits the search parameters
dbFetchItems:: Maybe Text                         -- ^ Part of name or description
            -> Maybe GeospatialPosition           -- ^ Location
            -> Maybe Double                       -- ^ Max distance from location
            -> Maybe Int                          -- ^ Max numbr of items
            ->Handler (Either String [(Key Item, Item, Maybe Double)]) -- ^ The result of the database search
dbFetchItems t p d limit = do
   item <- case p of
      Just _ -> do
         i1 <- runDB $ rawSql (sqlFetchBuildQuery t p d limit) (sqlFetchPlaceholder t p d limit)
         return $ clean1 <$> i1
      Nothing -> do
         i2 <- runDB $ rawSql (sqlFetchBuildQuery t p d limit) (sqlFetchPlaceholder t p d limit)
         return $ clean2 <$> i2
   return $ Right item
   where
      -- Clean up the query to return data that only returns key, item and distance
      clean1::(Entity Item, Single Double)->(Key Item, Item, Maybe Double)
      clean1 (Entity key dbitem,Single dd) = (key, dbitem, Just dd)

      -- Clean up the query to return data that only returns key, item and distance, in
      -- this spceific instance no distance
      clean2::Entity Item->(Key Item, Item, Maybe Double)
      clean2 (Entity key dbitem) = (key, dbitem, Nothing)

      -- Limit the number of returned items
      sqlFetchLimit::Maybe Int->Text
      sqlFetchLimit Nothing  = ""
      sqlFetchLimit (Just _) = " LIMIT ?"

      -- Create the placeholders for LIMIT
      sqlFetchLimitPlaceholder::Maybe Int->[PersistValue]
      sqlFetchLimitPlaceholder Nothing  = []
      sqlFetchLimitPlaceholder (Just i) = [PersistInt64 $ fromIntegral i]

      -- Build the query depending on what search parameters have been provided
      sqlFetchBuildQuery::Maybe Text->Maybe GeospatialPosition->Maybe Double->Maybe Int->Text
      sqlFetchBuildQuery Nothing Nothing _ l = "SELECT ?? FROM item ORDER BY name" <> sqlFetchLimit l
      sqlFetchBuildQuery (Just _) Nothing _ l = "SELECT ?? FROM item WHERE (name ILIKE ? OR description ILIKE ?) ORDER BY name" <> sqlFetchLimit l
      sqlFetchBuildQuery (Just _) (Just _) Nothing l = "SELECT ??, ST_Distance(position, ?, true) FROM item WHERE (name ILIKE ? OR description ILIKE ?) ORDER BY ST_Distance(position, ?, true)" <> sqlFetchLimit l
      sqlFetchBuildQuery (Just _) (Just _) (Just _) l = "SELECT ??, ST_Distance(position, ?, true) FROM item WHERE (name ILIKE ? OR description ILIKE ?) AND ST_DWithin(position,?,?,true) ORDER BY ST_Distance(position, ?, true)" <> sqlFetchLimit l
      sqlFetchBuildQuery Nothing (Just _) (Just _) l = "SELECT ??, ST_Distance(position, ?, true) FROM item WHERE ST_DWithin(position,?,?,true) ORDER BY ST_Distance(position, ?, true)" <> sqlFetchLimit l
      sqlFetchBuildQuery Nothing (Just _) Nothing l = "SELECT ??, ST_Distance(position, ?, true) FROM item ORDER BY ST_Distance(position, ?, true)" <> sqlFetchLimit l

      -- Build the placeholder depending on what search parameters have been provided
      sqlFetchPlaceholder::Maybe Text->Maybe GeospatialPosition->Maybe Double->Maybe Int->[PersistValue]
      sqlFetchPlaceholder Nothing Nothing _ l = sqlFetchLimitPlaceholder l
      sqlFetchPlaceholder (Just tt) Nothing _ l = [PersistText tt, PersistText tt] <> sqlFetchLimitPlaceholder l
      sqlFetchPlaceholder (Just tt) (Just pp) Nothing l = [toPersistValue pp, PersistText tt, PersistText tt, toPersistValue pp] <> sqlFetchLimitPlaceholder l
      sqlFetchPlaceholder (Just tt) (Just pp) (Just dd) l = [toPersistValue pp, PersistText tt, PersistText tt, toPersistValue pp, PersistDouble dd, toPersistValue pp] <> sqlFetchLimitPlaceholder l
      sqlFetchPlaceholder Nothing (Just pp) (Just dd) l = [toPersistValue pp, toPersistValue pp, PersistDouble dd, toPersistValue pp] <> sqlFetchLimitPlaceholder l
      sqlFetchPlaceholder Nothing (Just pp) Nothing l = [toPersistValue pp, toPersistValue pp] <> sqlFetchLimitPlaceholder l

-- | Creates the item in the database and return with the item and its keys
dbCreateItem:: Item                                                 -- ^ The Item
         -> Handler (Either String (Key Item, Item, Maybe Double))  -- ^ The result of the database search
dbCreateItem item = do
   key <- runDB $ insertBy item
   case key of
      Left (Entity k dbitem) -> return $ Right (k, dbitem, Nothing)
      Right k                -> return $ Right (k, item, Nothing)

dbUpdateItemAttributes::[(Maybe (Key AttributeValue), Maybe AttributeValue)]
   -> Handler (Either String ())
dbUpdateItemAttributes aav = do
   mapM_ (runDB . generate) aav
   pure $ Right ()
   where
      generate::(MonadIO m)=>(Maybe (Key AttributeValue), Maybe AttributeValue)->ReaderT SqlBackend m ()
      generate (Just k, Nothing) = delete k
      generate (Just k, Just av) = update k [AttributeValueValue =. attributeValueValue av]
      generate (Nothing, Just av) = void $ insert av
      generate (Nothing, Nothing) = pure ()

-- | Delete the item in the database
dbDeleteItem:: Key Item                       -- ^ The key
   ->Handler (Either String ())  -- ^ The result of the database search
dbDeleteItem key = do
   runDB $ delete key
   return $ Right ()

-- | Creates the item in the database and return with the complete item
dbUpdateItem:: Key Item       -- ^ The key
            ->[Update Item]   -- ^ Fields to update
            ->Handler (Either String (Maybe (Key Item, Item, Maybe Double)))  -- ^ The result of the database update
dbUpdateItem key items = do
   runDB $ update key items
   dbitem <- runDB $ get key
   case dbitem of
      Just d ->
         return $ Right $ Just (key, d, Nothing)
      Nothing ->
         return $ Right Nothing
