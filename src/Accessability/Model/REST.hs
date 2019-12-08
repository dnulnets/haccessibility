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
    PostItemsBody(..)) where

--
-- Import standard libs
--
import Data.Text (Text, pack)
import GHC.Generics (Generic(..))

--
-- JSON library
--
import Data.Aeson
import Data.Aeson.TH

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
    fieldLabelModifier = drop 9 -- Get rid of the 'postItem' in the field names
  } ''PostItemsBody)
