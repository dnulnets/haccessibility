{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

-- |
-- Module      : Heat.Data.Conversions
-- Description : Different types of data conversions functionality
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains different functions to convert from one type to another
module Accessability.Data.Conversions where

--
-- External imports
--
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Int

import Data.HexString

import Database.Persist
import Database.Persist.Types
import Database.Persist.Quasi
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Class

--
-- Internal imports
--
import Accessability.Model.DB

-- |Convert a Text string to a HexString, usually used to convert route parameters
toHex::Text        -- ^The hexadecimal representation
     ->HexString   -- ^The same representation but as a HexString
toHex = hexString . encodeUtf8

-- |Convert a Text string to a persistent key in the database, usually used to convert route parameters
toKey::ToBackendKey SqlBackend r => Text->Key r
toKey = toSqlKey . toBinary . toHex

-- |Convert a persistent key in the database to a Text string, usually used to convert keys to
-- fields in the body that are identifiers.
keyToHex::ToBackendKey SqlBackend r => Key r->HexString
keyToHex = fromBinary . fromSqlKey
