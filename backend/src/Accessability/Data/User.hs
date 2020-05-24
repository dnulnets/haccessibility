{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Acessability.Data.User
-- Description : The user type that are used by the application
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the user information regardless of interface.
--
module Accessability.Data.User
    ( User(..)
    )
where

--
-- Import standard libs
--
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic(..) )

--
-- JSON library
--
import           Data.Aeson
import           Data.Aeson.TH

--
-- Our own stuff
--

import           Accessability.Utils.JSON       ( firstLower )

--
-- User
--

-- | Definition of the user
data User = User {
    userId            :: Maybe Text -- ^ User key
    , userUsername        :: Text       -- ^ The username used when looging in
    , userPassword    :: Text       -- ^ The password, bcrypted
    , userEmail       :: Text       -- ^ The user email address
    } deriving (Generic)

--
-- JSON interfaces
--
--
-- JSON Option
--
-- customOptions :: Options
-- customOptions = defaultOptions

--
-- JSON for User
--

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'user' in the field names
  } ''User)
