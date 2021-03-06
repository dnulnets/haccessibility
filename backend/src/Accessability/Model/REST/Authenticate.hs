{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Heat.Interface.Authenticate
-- Description : The interface description for the login functionality
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the interfaces for the user handler API
--

module Accessability.Model.REST.Authenticate
  ( Authenticate(..)
  , UserInfo(..)
  , TokenInfo(..)
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
import Accessability.Data.User (Role)

-- |Authenticate body description, comes with the POST
data Authenticate = Authenticate
  { username :: Text  -- ^The username of the user
  , password :: Text -- ^The password to authenticate the user with
  } deriving (Generic, Show)

instance FromJSON Authenticate

-- |The JSON Web token returned after authentication, response to the POST
data UserInfo = UserInfo
             { iuserid   :: Text  -- ^Unique user identity
             , itoken    :: Text  -- ^The JSON Web token
             , iusername :: Text  -- ^The username
             , irole :: Role      -- ^The role of the user
             , iemail    :: Text     -- ^Email address to the user
             } deriving (Generic, Show)

-- |Automatically derive JSON code, but drop the first character of the fieldname, we do not want every one to begin with 'i'
$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 1 -- Get rid of the first character in the field names
     } ''UserInfo)

-- |Info contained in the token
data TokenInfo = TokenInfo
                { tiuserid  :: Text
                , tirole      :: Role
                } deriving (Generic, Show)

-- |Automatically derive JSON code, but drop the first character of the fieldname, we do not want every one to begin with 'i'
$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 2 -- Get rid of the first character in the field names
     } ''TokenInfo)

