{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Acessability.Model.Generic
-- Description : The types that are generic for all interfaces
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the common types regardless of interface or database
--
module Accessability.Model.Generic (
    ItemLevel(..),
    ItemSource(..),
    ItemState(..)) where

--
-- Import standard libs
--
import Data.Char (toLower)
import Data.Text (Text, pack)
import GHC.Generics (Generic(..))

--
-- JSON library
--
import Data.Aeson
import Data.Aeson.TH

--
-- Import for persistence
--
import Database.Persist
import Database.Persist.TH

--
-- JSON Option
--

customOptions = defaultOptions
                { constructorTagModifier = map $ toLower
                }

--
-- Enumeration ItemLevel
--

-- | The enumeration for the accessability level for an item
data ItemLevel = L1 | L2 | L3 | L4 | L5 deriving (Generic, Show, Read)

instance ToJSON ItemLevel where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON ItemLevel where
    parseJSON = genericParseJSON customOptions
    
derivePersistField "ItemLevel"

--
-- Enumeration ItemSource
--

-- | The enmueration for the source of the items state
data ItemSource = Manual | Automatic deriving (Generic, Show, Read)

instance ToJSON ItemSource where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON ItemSource where
    parseJSON = genericParseJSON customOptions
    
derivePersistField "ItemSource"

--
-- Enumeration ItemState
--

-- | The enmueration for the state of the item
data ItemState = Unknown | Online | Offline deriving (Generic, Show, Read)

instance ToJSON ItemState where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON ItemState where
    parseJSON = genericParseJSON customOptions
    
derivePersistField "ItemState"

--
-- Item
--

-- | Definition of the item
data Item = Item {
    itemID::Maybe Text          -- ^ Item key
    , itemName::Text            -- ^ The name of the item
    , itemDescription:: Text    -- ^ The description of the item
    , itemSource:: ItemSource   -- ^ How the items online state is determined
    , itemState:: ItemState     -- ^ The state of the item
    , itemLevel:: ItemLevel     -- ^ The accessability level of the item
    , itemLatitude:: Float      -- ^ The latitude of the item
    , itemLongitude:: Float     -- ^ The longitude of the item
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = drop 4 -- Get rid of the 'item' in the field names
  } ''Item)
