{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Acessability.Model.REST
-- Description : The REST API types
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the types for the graphQL support
--
module Accessability.Model.REST (
    PostItemsBody(..),
    PostItemBody(..),
    PutItemBody(..)) where

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
-- Our own
--
import Accessability.Model (
    ItemLevel(..),
    ItemState(..),
    ItemSource(..))
import Accessability.Utils.JSON (firstLower)

-- | The argument for the queryitems query
data PostItemsBody = PostItemsBody {
    postItemsLongitudeMin:: Maybe Float
    , postItemsLongitudeMax::Maybe Float
    , postItemsLatitudeMin::Maybe Float
    , postItemsLatitudeMax::Maybe Float
    , postItemsLimit::Maybe Int
    , postItemsText::Maybe Text
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 9 -- Get rid of the 'postItem' in the field names
  } ''PostItemsBody)

-- | The argument for the queryitems query
data PutItemBody = PutItemBody {
    putItemName::Maybe Text            -- ^ The name of the item
    , putItemDescription:: Maybe Text    -- ^ The description of the item
    , putItemSource:: Maybe ItemSource   -- ^ How the items online state is determined
    , putItemState:: Maybe ItemState     -- ^ The state of the item
    , putItemLevel:: Maybe ItemLevel     -- ^ The accessability level of the item
    , putItemLatitude:: Maybe Float      -- ^ The latitude of the item
    , putItemLongitude:: Maybe Float     -- ^ The longitude of the item
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 7 -- Get rid of the 'postItem' in the field names
  } ''PutItemBody)

  -- | The argument for the queryitems query
data PostItemBody = PostItemBody {
    postItemName::Text            -- ^ The name of the item
    , postItemDescription::Text    -- ^ The description of the item
    , postItemSource:: ItemSource   -- ^ How the items online state is determined
    , postItemState:: ItemState     -- ^ The state of the item
    , postItemLevel:: ItemLevel     -- ^ The accessability level of the item
    , postItemLatitude:: Float      -- ^ The latitude of the item
    , postItemLongitude:: Float     -- ^ The longitude of the item
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 8 -- Get rid of the 'postItem' in the field names
  } ''PostItemBody)