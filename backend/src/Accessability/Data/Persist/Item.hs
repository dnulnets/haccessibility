{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}

-- |
-- Module      : Acessability.Data.Persist.Item
-- Description : The types that are generic for all interfaces
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the common types regardless of interface or database
-- that is associated with geographical items.
--
module Accessability.Data.Persist.Item where

--
-- Import standard libs
--
import Data.Char (toLower)
import Data.Text (Text, pack)
import GHC.Generics (Generic(..))

--
-- Import for persistence
--
import Database.Persist
import Database.Persist.TH

--
-- Import of our own data type
--
import Accessability.Data.Item

--
-- Persistence for Enumeration ItemLevel
--

derivePersistField "ItemLevel"

--
-- Persistence for Enumeration ItemSource
--
    
derivePersistField "ItemSource"

--
-- Persistence for Enumeration ItemState
--
    
derivePersistField "ItemState"
