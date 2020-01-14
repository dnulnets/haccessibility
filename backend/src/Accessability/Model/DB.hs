{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Acessability.Model.DB
-- Description : The database model
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the database model and the haskell representation of
-- the data    
--
module Accessability.Model.DB where

--
-- Import standard libs
--
import Data.Text (Text)

--
-- Import for persistence
--
import Database.Persist
import Database.Persist.TH

--
-- Our own types
--
import Accessability.Data.Geo (Geo(..))
import Accessability.Model.GQL (ItemLevel(..), ItemState(..), ItemSource(..))
import Accessability.Data.Persist.Geo

-- Create migration function using both our entities and
-- serversession-backend-persistent ones.

-- | The database model
--
share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|

-- | The item, i.e. our base location
Item
    name Text           -- ^ Name of the item
    description Text    -- ^ Description of the location
    level ItemLevel     -- ^ The level of accessability
    source ItemSource   -- ^ The source of the state
    state ItemState     -- ^ The state of the item
    longitude Double    -- ^ Longitude in WGS84
    latitude Double     -- ^ Latitude in WGS84
    UniqueItemName name -- ^ The name is unique
    deriving Show

User
    username Text       -- ^ The user name
    password Text       -- ^ The password, bcrypted
    email Text          -- ^ The users email
    UniqueUserUsername username -- ^ The username is unique
    deriving Show

TestItem
    name Text
    pos Geo
    UniqueTestItemName name
    deriving Show
|]
