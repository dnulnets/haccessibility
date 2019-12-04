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
    MutationItemArgs(..)

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
import Data.Morpheus.Types    (GQLType(..), GQLScalar(..))

--
-- My own imports
--
import Accessability.Foundation (Handler)
import Accessability.Model.Geo (GeodeticPosition(..))

--
-- Enumeration ItemLevel
--

-- | The enumeration for the accessability level for an item
data ItemLevel = L1 | L2 | L3 | L4 | L5 deriving (Generic, Show, Read)

-- Make ItemLevel a GQL type
instance GQLType ItemLevel where
    type  KIND ItemLevel = ENUM
    description = const $ Just $ pack "The level of accessability of the item, L1-L5. L5 is the highest "

-- Make it possible to store this in the database
derivePersistField "ItemLevel"

--
-- Enumeration ItemSource
--

-- | The enmueration for the source of the items state
data ItemSource = Manual | Automatic deriving (Generic, Show, Read)

-- Make ItemSource a GQL type
instance GQLType ItemSource where
    type  KIND ItemSource = ENUM
    description = const $ Just $ pack "The source of the items state, i.e. if the items activity is manual or automatically determined"

-- Make it possible to store this in the database
derivePersistField "ItemSource"

--
-- Enumeration ItemState
--

-- | The enmueration for the state of the item
data ItemState = Unknown | Online | Offline deriving (Generic, Show, Read)

-- Make ItemLevel a GQL type
instance GQLType ItemState where
    type  KIND ItemState = ENUM
    description = const $ Just $ pack "The items state, i.e. if it is Online, Offline or Unknown"

-- Make it possible to store this in the database
derivePersistField "ItemState"

--
-- Object Item
--

-- | Definition of the item
data Item = Item { itemName::Text  -- ^ The name of the item
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
newtype Mutation m = Mutation {
    createItem :: MutationItemArgs -> m Item
  } deriving (Generic, GQLType)

-- | The argument for the queryitem query
data MutationItemArgs = MutationItemArgs {
        createItemName::Text            -- ^ The name of the item
        , createItemDescription:: Text  -- ^ The description of the item
        , createItemSource:: ItemSource -- ^ How the items online state is determined
        , createItemState:: ItemState   -- ^ The state of the item
        , createItemLevel:: ItemLevel   -- ^ The accessability level of the item
        , createItemLongitude:: Float  -- ^ The longitude of the location (WGS84)
        , createItemLatitude:: Float   -- ^ The latitude of the location (WGS84)
    } deriving (Generic)

--
-- Query object
--

-- | The graphQL Query type that contains all queries
data Query m = Query {

        -- | Queries a specific item
        queryItem:: QueryItemArgs         -- ^ The arguments for the query
                    -> m (Maybe Item)     -- ^ The found item

        , queryItems:: QueryItemsArgs
                    -> m ([Item])
    } deriving (Generic, GQLType)

-- | The argument for the queryitem query
data QueryItemArgs = QueryItemArgs {
    queryItemName      :: Text -- ^ The name of the item to search for
    } deriving (Generic)

-- | The argument for the queryitems query
data QueryItemsArgs = QueryItemsArgs {
    queryItemsLongitudeMin:: Float
    , queryItemsLongitudeMax::Float
    , queryItemsLatitudeMin::Float
    , queryItemsLatitudeMax::Float
    } deriving (Generic)

