{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Acessability.Model.Data
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
module Accessability.Model.Data where

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
import Accessability.Model.Geo (GeodeticPosition(..))
import Accessability.Model.GQL (ItemLevel(..), ItemState(..), ItemSource(..))

-- | The database model
--
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

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
|]
