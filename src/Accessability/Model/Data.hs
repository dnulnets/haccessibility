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
--
-- | The database model
--
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
    name Text
    description Text
    level ItemLevel
    source ItemSource
    state ItemState
    position GeodeticPosition

    UniqueItemName name
    deriving Show
|]
