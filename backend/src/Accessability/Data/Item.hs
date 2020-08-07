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
module Accessability.Data.Item
  ( Item(..)
  , ItemValue(..)
  , Attribute(..)
  , ItemSource(..)
  , ItemModifier(..)
  , ItemApproval(..)
  , AttributeType(..)
  )
where

--
-- Import standard libs
--
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic(..) )

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
import           Data.Morpheus.Kind             ( ENUM )
import           Data.Morpheus.Types            ( GQLType(..) )

--
-- Import our own stuff
--
import           Accessability.Utils.JSON       ( firstLower )

-- | The numberation for the attribute type
data AttributeType = TextType | NumberType | BooleanType deriving (Generic, Show, Read, Eq)

-- | The enmueration for the source of the item
data ItemSource = Human | Machine deriving (Generic, Show, Read, Eq)

-- | The enumeration for the modifier of the item
data ItemModifier = Static | Transient deriving (Generic, Show, Read, Eq)

-- | The enumeration for the approval of the item
data ItemApproval = Waiting | Approved | Denied deriving (Generic, Show, Read, Eq)

--
-- Item
--

-- | Definition of the item
data Item = Item {
    itemId            :: Maybe Text          -- ^ Item key
    , itemName        :: Text            -- ^ The short name of the item
    , itemGuid        :: Text            -- ^ The external unique identifier of the item
    , itemCreated     :: UTCTime     -- ^ The creation time
    , itemDescription :: Text    -- ^ The description of the item
    , itemSource      :: ItemSource   -- ^ How the items online state is determined
    , itemModifier    :: ItemModifier       -- ^ The type of the item
    , itemApproval    :: ItemApproval   -- ^ The tstae of approval for the item
    , itemLatitude    :: Float      -- ^ The latitude of the item
    , itemLongitude   :: Float     -- ^ The longitude of the item
    , itemDistance    :: Maybe Float -- ^ The distance to a specified point at the time of the query
    , itemPositive    :: Maybe Integer      -- ^ Number of positive user properties
    , itemNegative    :: Maybe Integer      -- ^ Number of negative user properties
    , itemUnknown     :: Maybe Integer      -- ^ Number of unknown user properties
    } deriving (Generic, Show)

-- | Definition of the attribute and it doubles as the value for an attribute and item
data Attribute = Attribute {
    attributeAttributeId   :: Maybe Text      -- ^Attribute key
    , attributeName        :: Text            -- ^The name of the attribute
    , attributeDisplayName :: Text            -- ^The name to display for this attribute
    , attributeGroup       :: Text            -- ^The group the atribute belongs to
    , attributeDescription :: Text            -- ^The description of the attribute 
    , attributeTypeof      :: AttributeType   -- ^The type of the attribute
    , attributeUnit        :: Text            -- ^The unit of the attribute
    , attributeValue       :: Maybe Text      -- ^The value of the attribute
    , attributeItemId      :: Maybe Text      -- ^The key to the item that the AttributeValue belongs to
    , attributeAttributeValueId :: Maybe Text -- ^The key to the AtributeValue record
    } deriving (Generic, Show)

-- | Definition of the value for an item with respect to the users property
data ItemValue = ItemValue {
      positive    :: Integer
      , negative  :: Integer
      , unknown   :: Integer
    } deriving (Show)

instance Monoid ItemValue where
  mempty = ItemValue { positive = 0, negative = 0, unknown = 0}

instance Semigroup ItemValue where
  (<>) i1 i2 = ItemValue {positive = positive i1 + positive i2,
                          negative = negative i1 + negative i2,
                          unknown = unknown i1 + unknown i2}

--
-- Persistence
--

--
-- Persistence for Enumberation AttributeType
--
derivePersistField "AttributeType"

--
-- Persistence for Enumeration ItemSource
--

derivePersistField "ItemSource"

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

-- Make ItemSource a GQL type
instance GQLType ItemSource where
  type KIND ItemSource = ENUM
  description =
    const
      $ Just
      $ pack
          "The source of the items state, i.e. if the items activity is manual or automatically determined"

-- Make ItemLevel a GQL type
instance GQLType ItemModifier where
  type KIND ItemModifier = ENUM
  description =
    const $ Just $ pack "The items modifier, i.e. if it is Static or Transient"

-- Make ItemLevel a GQL type
instance GQLType ItemApproval where
  type KIND ItemApproval = ENUM
  description = const $ Just $ pack
    "The items approval, i.e. if it is Waiting, Approved or Denied"

--
-- JSON interfaces
--
--
-- JSON Option
--
customOptions :: Options
customOptions = defaultOptions

--
-- JSON for Enumeration AttributeType
--

instance ToJSON AttributeType where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON AttributeType where
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

-- |Automatically derive JSON but we do not want the first characters in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'item' in the field names
  } ''Item)

--
-- JSON for Attribute
--

-- |Automatically derive JSON but we do not want the first characters in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 9 -- Get rid of the 'attribute' in the field names
  } ''Attribute)
