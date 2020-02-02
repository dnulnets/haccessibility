{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Acessability.Data.Item
-- Description : The item type that are used by the application
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the common item type regardless of interface or database
-- that is associated with geographical items.
--
module Accessability.Data.Item (
    Item(..),
    ItemLevel(..),
    ItemSource(..),
    ItemState(..),
    ItemModifier(..),
    ItemApproval(..)) where

--
-- Import standard libs
--
import           Data.Text                (Text, pack)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics             (Generic (..))

--
-- Import for persistence
--
import           Database.Persist.TH

--
-- JSON library
--
import           Data.Aeson
import           Data.Aeson.TH

--
-- Imports for GQL
--
import           Data.Morpheus.Kind       (ENUM)
import           Data.Morpheus.Types      (GQLType (..))

--
-- Import our own stuff
--
import           Accessability.Utils.JSON (firstLower)

-- | The enumeration for the accessability level for an item
data ItemLevel = L1 | L2 | L3 | L4 | L5 deriving (Generic, Show, Read)

-- | The enmueration for the source of the item
data ItemSource = Human | Machine deriving (Generic, Show, Read)

-- | The enumeration for the modifier of the item
data ItemModifier = Static | Transient deriving (Generic, Show, Read)

-- | The enumeration for the approval of the item
data ItemApproval = Waiting | Approved | Denied deriving (Generic, Show, Read)

-- | The enmueration for the state of the item
data ItemState = Online | Unknown | Offline deriving (Generic, Show, Read)

--
-- Item
--

-- | Definition of the item
data Item = Item {
    itemId            :: Maybe Text          -- ^ Item key
    , itemName        :: Text            -- ^ The short name of the item
    , itemGuid        :: Text            -- ^ The external unique identifier of the item
    , itemCreated     :: UTCTime     -- ^ The creation time
    , itemDescription ::  Text    -- ^ The description of the item
    , itemSource      ::  ItemSource   -- ^ How the items online state is determined
    , itemState       ::  ItemState     -- ^ The state of the item
    , itemLevel       ::  ItemLevel     -- ^ The accessability level of the item
    , itemModifier    :: ItemModifier       -- ^ The type of the item
    , itemApproval    :: ItemApproval   -- ^ The tstae of approval for the item
    , itemLatitude    ::  Float      -- ^ The latitude of the item
    , itemLongitude   ::  Float     -- ^ The longitude of the item
    , itemDistance    :: Maybe Float -- ^ The distance to a specified point at the time of the query
    } deriving (Generic)

--
-- Persistence
--

--
-- Persistence for Enumeration ItemLevel
--

derivePersistField "ItemLevel"

--
-- Persistence for Enumeration ItemSource
--

derivePersistField "ItemSource"

--
-- Persistence for Enumeration ItemState
--

derivePersistField "ItemState"

--
-- Persistence for Enumeration ItemModifier
--

derivePersistField "ItemModifier"

--
-- Persistence for Enumeration ItemApproval
--

derivePersistField "ItemApproval"


--
-- GQL Instances
--

-- Make ItemLevel a GQL type
instance GQLType ItemLevel where
    type  KIND ItemLevel = ENUM
    description = const $ Just $ pack "The level of accessability of the item, L1-L5. L5 is the highest "

-- Make ItemSource a GQL type
instance GQLType ItemSource where
    type  KIND ItemSource = ENUM
    description = const $ Just $ pack "The source of the items state, i.e. if the items activity is manual or automatically determined"

-- Make ItemLevel a GQL type
instance GQLType ItemState where
    type  KIND ItemState = ENUM
    description = const $ Just $ pack "The items state, i.e. if it is Online, Offline or Unknown"

-- Make ItemLevel a GQL type
instance GQLType ItemModifier where
    type  KIND ItemModifier = ENUM
    description = const $ Just $ pack "The items modifier, i.e. if it is Static or Transient"

-- Make ItemLevel a GQL type
instance GQLType ItemApproval where
    type  KIND ItemApproval = ENUM
    description = const $ Just $ pack "The items approval, i.e. if it is Waiting, Approved or Denied"

--
-- JSON interfaces
--
--
-- JSON Option
--
customOptions :: Options
customOptions = defaultOptions

--
-- JSON for Enumeration ItemLevel
--

instance ToJSON ItemLevel where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON ItemLevel where
    parseJSON = genericParseJSON customOptions

--
-- JSON for Enumeration ItemSource
--

instance ToJSON ItemSource where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON ItemSource where
    parseJSON = genericParseJSON customOptions

--
-- JSON for Enumeration ItemState
--

instance ToJSON ItemState where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON ItemState where
    parseJSON = genericParseJSON customOptions

--
-- JSON for Enumeration ItemModifier
--

instance ToJSON ItemModifier where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON ItemModifier where
    parseJSON = genericParseJSON customOptions

--
-- JSON for Enumeration ItemApproval
--

instance ToJSON ItemApproval where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON ItemApproval where
    parseJSON = genericParseJSON customOptions

--
-- JSON for Item
--

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'item' in the field names
  } ''Item)
