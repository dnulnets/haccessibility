{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}

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
    ItemSource(..),

    Mutation(..),
    MutationCreateItemArgs(..),
    MutationDeleteItemArgs(..),
    MutationUpdateItemArgs(..)

    ) where

--
-- Import standard libs
--
import qualified Data.Aeson              as DA
import qualified Data.ByteString.Lazy    as DBL
import           Data.Text               (Text, pack)
import           Data.Text.Encoding      (decodeUtf8)
import           Data.Text.Lazy          (fromStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock         (UTCTime)
import           GHC.Generics            (Generic (..))

--
-- Import for morpheus
--
import           Data.Morpheus.Kind      (SCALAR)
import           Data.Morpheus.Types     (GQLScalar (..), GQLType (..), ID (..),
                                          ScalarValue (..))

--
-- My own imports
--
import           Accessability.Data.Item (ItemApproval (..), 
                                          ItemModifier (..), ItemSource (..))

--
-- Scalars
--
--
-- GQL
--
instance GQLType UTCTime where
    type  KIND UTCTime = SCALAR
    description = const $ Just $ pack "The type that holds the UTC timestamp scalar"

instance GQLScalar UTCTime where

    parseValue (Int _) = Left "Wrong type for UTC timestamp"
    parseValue (Float _) = Left "Wrong type for UTC timestamp"
    parseValue (Boolean _) = Left "Wrong type for UTC timestamp"
    parseValue (String s) = case DA.eitherDecode $ encodeUtf8 $ fromStrict ("\"" <> s <> "\"") of
                                (Right u) -> Right u
                                (Left e)  -> Left $ pack $ "Unable to parse " <> e


    --serialize u = String $ pack $ show u
    serialize u = String $ decodeUtf8 $ DBL.toStrict $ DBL.init . DBL.tail $ DA.encode u
--
-- Object Item
--

-- | Definition of the item
data Item = Item {
    itemId            :: Maybe ID  -- ^ The ID of the item
    , itemName        :: Text  -- ^ The name of the item
    , itemGuid        :: Text  -- ^ The external unique identifier of the item
    , itemDescription :: Text       -- ^ The description of the item
    , itemSource      :: ItemSource      -- ^ How the items online state is determined
    , itemModifier    :: ItemModifier     -- ^ The modifier of the item
    , itemApproval    :: ItemApproval     -- ^ The approval state of the item
    , itemLatitude    :: Float        -- ^ The latitude of the item
    , itemLongitude   :: Float       -- ^ The longitude of the item
    , itemCreated     :: UTCTime   -- ^ The zoned time of the item
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
        , createItemGuid        :: Text           -- ^ The global unique identifier
        , createItemDescription :: Text  -- ^ The description of the item
        , createItemSource      :: ItemSource -- ^ How the items online state is determined
        , createItemModifier    :: ItemModifier -- ^ The modifier of the item
        , createItemApproval    :: ItemApproval -- ^ The approval state of the item
        , createItemCreated     :: UTCTime    -- ^ The create date of the item
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
        , updateItemName        :: Maybe Text            -- ^ The name of the item
        , updateItemGuid        :: Maybe Text           -- ^ The external unique identifier
        , updateItemDescription :: Maybe Text  -- ^ The description of the item
        , updateItemSource      :: Maybe ItemSource -- ^ How the items online state is determined
        , updateItemModifier    :: Maybe ItemModifier -- ^ The modifier of the item
        , updateItemApproval    :: Maybe ItemApproval -- ^ The approval state of the item
        , updateItemCreated     :: Maybe UTCTime   -- ^ The create time of the item
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

