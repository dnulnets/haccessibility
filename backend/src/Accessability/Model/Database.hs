{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

-- |
-- Module      : Acessability.Model.Database
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
module Accessability.Model.Database where

--
-- Import standard libs
--
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic(..) )
--
-- Import for persistence
--
import           Database.Persist.TH

--
-- Our own types
--
import           Accessability.Data.Geo         ( GeospatialPosition(..) )
import           Accessability.Data.Item        ( ItemApproval(..)
                                                , ItemLevel(..)
                                                , ItemModifier(..)
                                                , ItemSource(..)
                                                , ItemState(..)
                                                , AttributeType(..)
                                                )

-- Create migration function using both our entities and
-- serversession-backend-persistent ones.

-- | The database model
--
share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|

-- | The item, i.e. our base location
Item
    name Text           -- ^ Name of the item
    guid Text           -- ^ The global unique identifier of the item
    description Text    -- ^ Description of the location
    level ItemLevel     -- ^ The level of accessability
    source ItemSource   -- ^ The source of the state
    state ItemState     -- ^ The state of the item
    modifier ItemModifier   -- ^ The modifier of the item
    approval ItemApproval   -- ^ The approval state of the item
    position GeospatialPosition -- ^ The geodetical position
    created UTCTime   -- ^ The created date
    UniqueItemGuid guid -- ^ The guid is unique
    deriving Show Generic

-- | The available attributes
Attribute
    name Text           -- ^The name of the attribute, must be unique
    description Text    -- ^The description of the attribute
    typeof AttributeType         -- ^The type of value ATText|ATYesNo|ATNumber
    unit Text           -- ^The unit of the attribute, e.g. meters
    UniqueAttributeName -- ^The name is UniqueItemGuid

AttributeValue
    attribute AttributeId -- ^The attribute whose value is stored here
    item ItemId           -- ^The item whose value is stored here
    value Text            -- ^The value

User
    username Text       -- ^ The user name
    password Text       -- ^ The password, bcrypted
    email Text          -- ^ The users email
    UniqueUserUsername username -- ^ The username is unique
    deriving Show Generic

|]
