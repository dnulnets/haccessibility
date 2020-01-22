{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Acessability.Model.GQL
-- Description : The graphQL API model
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the types for the graphQL support
--
module Accessability.Model.GQL (
    Query(..),
    QueryItemArgs(..),
    QueryItemsArgs(..),
    Item(..),
    ItemLevel(..),
    ItemState(..),
    ItemSource(..),

    Mutation(..),
    MutationCreateItemArgs(..),
    MutationDeleteItemArgs(..),
    MutationUpdateItemArgs(..)

    ) where

--
-- Import standard libs
--
import           Data.Text               (Text, pack)
import           GHC.Generics            (Generic (..))

--
-- Import for morpheus
--
import           Data.Morpheus.Types     (GQLType (..), ID (..))

--
-- My own imports
--
import           Accessability.Data.Item (ItemLevel (..), ItemSource (..),
                                          ItemState (..))

--
-- Object Item
--

-- | Definition of the item
data Item = Item {
    itemId            :: Maybe ID  -- ^ The ID of the item
    , itemName        :: Text  -- ^ The name of the item
    , itemDescription :: Text       -- ^ The description of the item
    , itemSource      :: ItemSource      -- ^ How the items online state is determined
    , itemState       :: ItemState        -- ^ The state of the item
    , itemLevel       :: ItemLevel        -- ^ The accessability level of the item
    , itemLatitude    :: Float        -- ^ The latitude of the item
    , itemLongitude   :: Float       -- ^ The longitude of the item
    , itemDistance    :: Maybe Float -- ^ The distance from a specified point provided by the query
    } deriving (Generic)

-- Make Item a GQL Type
instance GQLType Item where
    description = const $ Just $ pack "The item that holds the accessability information"

--
-- Mutation object
--

-- | The graphQL Mutation type that contains all mutations
data Mutation m = Mutation {
    createItem   :: MutationCreateItemArgs -> m Item
    , deleteItem :: MutationDeleteItemArgs -> m (Maybe Item)
    , updateItem :: MutationUpdateItemArgs -> m (Maybe Item)
  } deriving (Generic, GQLType)

-- | The argument for the queryitem query
data MutationCreateItemArgs = MutationCreateItemArgs {
        createItemName          ::Text            -- ^ The name of the item
        , createItemDescription :: Text  -- ^ The description of the item
        , createItemSource      :: ItemSource -- ^ How the items online state is determined
        , createItemState       :: ItemState   -- ^ The state of the item
        , createItemLevel       :: ItemLevel   -- ^ The accessability level of the item
        , createItemLongitude   :: Float  -- ^ The longitude of the location (WGS84)
        , createItemLatitude    :: Float   -- ^ The latitude of the location (WGS84)
    } deriving (Generic)

-- | The argument for the queryitem query
newtype MutationDeleteItemArgs = MutationDeleteItemArgs {
        deleteItemId :: ID            -- ^ The key of the item
    } deriving (Generic)

-- | The argument for the queryitem query
data MutationUpdateItemArgs = MutationUpdateItemArgs {
        updateItemId            :: ID
        , updateItemName        ::Maybe Text            -- ^ The name of the item
        , updateItemDescription :: Maybe Text  -- ^ The description of the item
        , updateItemSource      :: Maybe ItemSource -- ^ How the items online state is determined
        , updateItemState       :: Maybe ItemState   -- ^ The state of the item
        , updateItemLevel       :: Maybe ItemLevel   -- ^ The accessability level of the item
        , updateItemLongitude   :: Maybe Float  -- ^ The longitude of the location (WGS84)
        , updateItemLatitude    :: Maybe Float   -- ^ The latitude of the location (WGS84)
    } deriving (Generic)

--
-- Query object
--

-- | The graphQL Query type that contains all queries
data Query m = Query {

        -- | Queries a specific item
        queryItem:: QueryItemArgs         -- ^ The arguments for the query
                    -> m (Maybe Item)     -- ^ The found item

        , queryItems:: QueryItemsArgs   -- ^ The arguments for the query
                    -> m [Item]       -- ^ The list of found items

    } deriving (Generic, GQLType)

-- | The argument for the queryitem query
newtype QueryItemArgs = QueryItemArgs {
    queryItemId      :: ID -- ^ The identifier
    } deriving (Generic)

-- | The argument for the queryitems query
data QueryItemsArgs = QueryItemsArgs {
    queryItemsLongitude  :: Maybe Float
    , queryItemsLatitude :: Maybe Float
    , queryItemsDistance :: Maybe Float
    , queryItemsLimit    :: Maybe Int
    , queryItemsText     :: Maybe Text
    } deriving (Generic)

