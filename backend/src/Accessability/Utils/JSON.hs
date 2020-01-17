-- |
-- Module      : Acessability.Utils.JSON
-- Description : Helper functions for JSON translation
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains helper functions for JSON translations
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
