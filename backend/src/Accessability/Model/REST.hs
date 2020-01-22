{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Acessability.Model.REST
-- Description : The REST API types
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the types for the REST API
--
module Accessability.Model.REST (
    PostItemsBody(..),
    PostItemBody(..),
    PutItemBody(..)) where

--
-- Import standard libs
--
import           Data.Text                (Text)
import           GHC.Generics             (Generic (..))

--
-- JSON library
--
import           Data.Aeson
import           Data.Aeson.TH

--
-- Our own
--
import           Accessability.Data.Item  (ItemLevel (..), ItemSource (..),
                                           ItemState (..))
import           Accessability.Utils.JSON (firstLower)

-- | The argument for the queryitems query
data PostItemsBody = PostItemsBody {
    postItemsLongitude  ::  Maybe Float -- ^ The longitude of search circle
    , postItemsLatitude :: Maybe Float  -- ^ The latitude of the search circle
    , postItemsDistance :: Maybe Float  -- ^ The distance or size of the search circle
    , postItemsLimit    :: Maybe Int -- ^ Max number of items
    , postItemsText     :: Maybe Text -- ^ The text that must be present in name or description
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 9 -- Get rid of the 'postItem' in the field names
  } ''PostItemsBody)

-- | The argument for the queryitems query
data PutItemBody = PutItemBody {
    putItemName          :: Maybe Text            -- ^ The name of the item
    , putItemDescription ::  Maybe Text    -- ^ The description of the item
    , putItemSource      ::  Maybe ItemSource   -- ^ How the items online state is determined
    , putItemState       ::  Maybe ItemState     -- ^ The state of the item
    , putItemLevel       ::  Maybe ItemLevel     -- ^ The accessability level of the item
    , putItemLatitude    ::  Maybe Float      -- ^ The latitude of the item
    , putItemLongitude   ::  Maybe Float     -- ^ The longitude of the item
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 7 -- Get rid of the 'postItem' in the field names
  } ''PutItemBody)

  -- | The argument for the queryitems query
data PostItemBody = PostItemBody {
    postItemName          :: Text            -- ^ The name of the item
    , postItemDescription :: Text    -- ^ The description of the item
    , postItemSource      ::  ItemSource   -- ^ How the items online state is determined
    , postItemState       ::  ItemState     -- ^ The state of the item
    , postItemLevel       ::  ItemLevel     -- ^ The accessability level of the item
    , postItemLatitude    ::  Float      -- ^ The latitude of the item
    , postItemLongitude   ::  Float     -- ^ The longitude of the item
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 8 -- Get rid of the 'postItem' in the field names
  } ''PostItemBody)
