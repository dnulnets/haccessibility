{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}

-- |
-- Module      : Acessability.Data.Item
-- Description : The types that are generic for all interfaces
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the common types regardless of interface or database
-- that is associated with geographical items.
--
module Accessability.Data.Item (
    Item(..),
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
-- Enumeration ItemLevel
--

-- | The enumeration for the accessability level for an item
data ItemLevel = L1 | L2 | L3 | L4 | L5 deriving (Generic, Show, Read)

--
-- Enumeration ItemSource
--

-- | The enmueration for the source of the items state
data ItemSource = Manual | Automatic deriving (Generic, Show, Read)

--
-- Enumeration ItemState
--

-- | The enmueration for the state of the item
data ItemState = Unknown | Online | Offline deriving (Generic, Show, Read)

--
-- Item
--

-- | Definition of the item
data Item = Item {
    itemId::Maybe Text          -- ^ Item key
    , itemName::Text            -- ^ The name of the item
    , itemDescription:: Text    -- ^ The description of the item
    , itemSource:: ItemSource   -- ^ How the items online state is determined
    , itemState:: ItemState     -- ^ The state of the item
    , itemLevel:: ItemLevel     -- ^ The accessability level of the item
    , itemLatitude:: Float      -- ^ The latitude of the item
    , itemLongitude:: Float     -- ^ The longitude of the item
    } deriving (Generic)
