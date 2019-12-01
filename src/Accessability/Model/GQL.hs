{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}

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
    Item(..),
    ItemLevel(..)) where

--
-- Import standard libs
--
import Data.Text (Text, pack)
import GHC.Generics (Generic(..))

--
-- Import for morpheus
--

import Data.Morpheus.Kind     (SCALAR, OBJECT, ENUM)
import Data.Morpheus.Types    (GQLType(..))
--
-- My own imports
--
import Accessability.Foundation (Handler)
import Accessability.Model.Geo (GeodeticPosition(..))

-- | The graphQL Query type that contains all queries
data Query m = Query {

        -- | Queries a specific item
        queryItem:: QueryItemArgs -- ^ The arguments for the query
                    -> m Item     -- ^ The found item
    } deriving (Generic, GQLType)

-- | The argument for the queryitem query
data QueryItemArgs = QueryItemArgs
    { queryItemArgsName      :: Text -- ^ The name of the item to search for
    } deriving (Generic)

-- | The enmueration for the accessability level for an item
data ItemLevel = L1 | L2 | L3 | L4 | L5 deriving (Generic)

-- Make ItemLevel a GQL type
instance GQLType ItemLevel where
    type  KIND ItemLevel = ENUM
    description = const $ Just $ pack "The level of accessability of the item, L1-L5. L5 is the highest "

-- | Definition of the item
data Item = Item {
    itemName        :: Text                 -- ^ The name of the item
    , itemLevel     :: ItemLevel            -- ^ The accessability level of the item
    , itemPosition  :: GeodeticPosition     -- ^ The geographical position of the item
    } deriving (Generic)

-- Make Item a GQL Type
instance GQLType Item where
    type  KIND Item = OBJECT
    description = const $ Just $ pack "The item that holds the accessability information"
    