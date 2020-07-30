{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Accessability.Model.REST.User
-- Description : The REST interface for a User
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the interfaces for the user handler API
--

module Accessability.Model.REST.User
  ( PutUserProperty(..)
  )
where

--
-- External imports
--
import           GHC.Generics                   ( Generic )

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                      ( Text )

--
-- Heat imports
--
import           Accessability.Data.User        ( Operation(..))

-- |The JSON Web token returned after authentication, response to the POST
data PutUserProperty = PutUserProperty {
    upuserPropertyId::Maybe Text
    , upattributeId::Maybe Text
    , upoperation::Maybe Operation
    , upnegate::Maybe Bool
    , upvalue:: Maybe Text
    , upvalue1::Maybe Text } deriving (Generic, Show)

-- |Automatically derive JSON code, but drop the first character of the fieldname, we do not want every one to begin with 'i'
$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 2 -- Get rid of the first character in the field names
     } ''PutUserProperty)
