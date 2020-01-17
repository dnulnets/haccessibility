{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Acessability.Data.Json.Item
-- Description : The JSON implementation of the Item data type
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the JSON implementation of the Item data type.
--
module Accessability.Data.Json.Item where

--
-- Import standard libs
--
import Data.Char (toLower)
import Data.Text (Text, pack)

--
-- JSON library
--
import Data.Aeson
import Data.Aeson.TH

--
-- Import our own stuff
--
import Accessability.Data.Item
import Accessability.Utils.JSON (firstLower)

--
-- JSON Option
--
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
-- JSON for Item
--

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'item' in the field names
  } ''Item)