-- |
-- Module      : Accessibility.Utils.Password
-- Description : Password handling
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains functionality to generate and verify bcrypted passwords.
--
module Accessability.Utils.Password (authHashPassword, authValidatePassword) where

--
-- External imports
--
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import Crypto.KDF.BCrypt (hashPassword,validatePassword)

-- |Validates a password by checking a hashed password with the supplied password
authValidatePassword::Text -- ^The hashed password
                    ->Text -- ^The user supplied password in clear text
                    ->Bool -- ^True if they match, otherwise false
authValidatePassword hpwd upwd = validatePassword (encodeUtf8 upwd) (encodeUtf8 hpwd) 

-- |Hashes a password.
authHashPassword :: Integer       -- ^The cost of the hashing work
                 -> Text          -- ^The user supplied password in clear text
                 ->IO ByteString  -- ^The hashed password
authHashPassword cost pwd = hashPassword (fromIntegral cost) (encodeUtf8 pwd)

  -- "$2b$10$jRs4Mriaz0BMBljpRc1NyO3/DSQP4J6Fco6izBU2dfbREaLcM6Vwy"
  