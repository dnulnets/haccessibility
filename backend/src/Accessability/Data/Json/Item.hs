{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

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
module Accessability.Data.Json.Item where

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
-- Import our own stuff
--
import Accessability.Utils.JSON (firstLower)
import Accessability.Data.Item

--
-- JSON Option
--
customOptions = defaultOptions

--
-- Enumeration ItemLevel
--

instance ToJSON ItemLevel where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON ItemLevel where
    parseJSON = genericParseJSON customOptions
    
--
-- Enumeration ItemSource
--

instance ToJSON ItemSource where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON ItemSource where
    parseJSON = genericParseJSON customOptions

--
-- Enumeration ItemState
--

instance ToJSON ItemState where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON ItemState where
    parseJSON = genericParseJSON customOptions

--
-- Item
--

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'item' in the field names
  } ''Item)