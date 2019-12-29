-- |
-- Module      : Acessability.Model.Transform
-- Description : The database model
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains functions that transform from database to gql primitives
--
module Accessability.Utils.JSON (
    firstLower) where

--
-- Standard libs
--
import Data.Char (toLower)

-- | Make the strings first character lowercase
firstLower :: String -> String
firstLower (head:tail) = toLower head : tail
firstLower [] = []
