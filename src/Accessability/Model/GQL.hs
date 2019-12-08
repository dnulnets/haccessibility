{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Acessability.Model.GQL
-- Description : The graphQL API entrypoint
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
import Data.Text (Text, pack)
import GHC.Generics (Generic(..))

--
-- To be bale to generate the persist field
--
import Database.Persist
import Database.Persist.TH

--
-- Import for morpheus
--

import Data.Morpheus.Kind     (SCALAR, OBJECT, ENUM)
import Data.Morpheus.Types    (GQLType(..), GQLScalar(..), ID(..))

--
-- My own imports
--
import Accessability.Foundation (Handler)
import Accessability.Model.Geo (GeodeticPosition(..))
import Accessability.Model.Generic

--
-- Enumeration ItemLevel
--

-- Make ItemLevel a GQL type
instance GQLType ItemLevel where
    type  KIND ItemLevel = ENUM
    description = const $ Just $ pack "The level of accessability of the item, L1-L5. L5 is the highest "

--
-- Enumeration ItemSource
--

-- Make ItemSource a GQL type
instance GQLType ItemSource where
    type  KIND ItemSource = ENUM
    description = const $ Just $ pack "The source of the items state, i.e. if the items activity is manual or automatically determined"

--
-- Enumeration ItemState
--

-- Make ItemLevel a GQL type
instance GQLType ItemState where
    type  KIND ItemState = ENUM
    description = const $ Just $ pack "The items state, i.e. if it is Online, Offline or Unknown"

--
-- Object Item
--

-- | Definition of the item
data Item = Item { 
    itemID::Maybe ID
    , itemName::Text  -- ^ The name of the item
    , itemDescription:: Text       -- ^ The description of the item
    , itemSource:: ItemSource      -- ^ How the items online state is determined
    , itemState:: ItemState        -- ^ The state of the item
    , itemLevel:: ItemLevel        -- ^ The accessability level of the item
    , itemLatitude:: Float        -- ^ The latitude of the item
    , itemLongitude:: Float       -- ^ The longitude of the item
    } deriving (Generic)

-- Make Item a GQL Type
instance GQLType Item where
    type  KIND Item = OBJECT
    description = const $ Just $ pack "The item that holds the accessability information"
    
--
-- Mutation object
--

-- | The graphQL Mutation type that contains all mutations
data Mutation m = Mutation {
    createItem :: MutationCreateItemArgs -> m Item
    , deleteItem :: MutationDeleteItemArgs -> m (Maybe Item)
    , updateItem :: MutationUpdateItemArgs -> m (Maybe Item)
  } deriving (Generic, GQLType)

-- | The argument for the queryitem query
data MutationCreateItemArgs = MutationCreateItemArgs {
        createItemName::Text            -- ^ The name of the item
        , createItemDescription:: Text  -- ^ The description of the item
        , createItemSource:: ItemSource -- ^ How the items online state is determined
        , createItemState:: ItemState   -- ^ The state of the item
        , createItemLevel:: ItemLevel   -- ^ The accessability level of the item
        , createItemLongitude:: Float  -- ^ The longitude of the location (WGS84)
        , createItemLatitude:: Float   -- ^ The latitude of the location (WGS84)
    } deriving (Generic)

-- | The argument for the queryitem query
data MutationDeleteItemArgs = MutationDeleteItemArgs {
        deleteItemID::ID            -- ^ The key of the item
    } deriving (Generic)

-- | The argument for the queryitem query
data MutationUpdateItemArgs = MutationUpdateItemArgs {
        updateItemID::ID
        , updateItemName::Maybe Text            -- ^ The name of the item
        , updateItemDescription:: Maybe Text  -- ^ The description of the item
        , updateItemSource:: Maybe ItemSource -- ^ How the items online state is determined
        , updateItemState:: Maybe ItemState   -- ^ The state of the item
        , updateItemLevel:: Maybe ItemLevel   -- ^ The accessability level of the item
        , updateItemLongitude:: Maybe Float  -- ^ The longitude of the location (WGS84)
        , updateItemLatitude:: Maybe Float   -- ^ The latitude of the location (WGS84)
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
                    -> m ([Item])       -- ^ The list of found items

    } deriving (Generic, GQLType)

-- | The argument for the queryitem query
data QueryItemArgs = QueryItemArgs {
    queryItemID      :: ID -- ^ The identifier
    } deriving (Generic)

-- | The argument for the queryitems query
data QueryItemsArgs = QueryItemsArgs {
    queryItemsLongitudeMin:: Maybe Float
    , queryItemsLongitudeMax::Maybe Float
    , queryItemsLatitudeMin::Maybe Float
    , queryItemsLatitudeMax::Maybe Float
    , queryItemsLimit::Maybe Int
    , queryItemsText::Maybe Text
    } deriving (Generic)

