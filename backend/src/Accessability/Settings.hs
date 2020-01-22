{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Accessibility.Settings
-- Description : The application settings
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the possible settings for the application and also
-- a default setting.
--
module Accessability.Settings (
  AppSettings(..),
  defaultSettings) where

--
-- External imports
--
import           Data.Text (Text)

-- |Our application settings
data AppSettings = AppSettings {

  tokenSecret       :: Text            -- ^The secret used to sign and verify a JSON Web Token
  , tokenExpiration :: Integer   -- ^The expiration time of the token in seconds
  , passwordCost    :: Integer       -- ^The cost for the bcrypt password hash generation

  }

-- |A default setting for our application
defaultSettings::AppSettings
defaultSettings = AppSettings {

  tokenSecret = "mandelmassa"   -- ^The default token secret
  ,tokenExpiration = 60*60      -- ^The default token is only valid for 1 hour
  ,passwordCost = 10            -- ^The default password hashing cost

  }
