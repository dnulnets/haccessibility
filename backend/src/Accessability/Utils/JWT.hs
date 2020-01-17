{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Heat.Utils.JWT
-- Description : JSON Web Token functions
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains functions to create and verify JSON Web Token
-- and to extract and insert the unregistered claim as a JSON structure
-- where you can store relevant information for your application
module Accessability.Utils.JWT (jsonToToken,
                                tokenToJson) where

--
-- External Library imports
--
import Data.Text (Text)
import Data.Map as Map (fromList, (!?))
import Data.Aeson (Value)
import Data.Time.Clock (NominalDiffTime)
import Web.JWT as JWT

-- | The name of the unregistered claim in the JSON Web Token
key :: Text -- ^ The name of the key
key = "info"

-- | Create a token out of a given JSON 'Value'
jsonToToken :: Text  -- ^ The secret used for signing
            -> NominalDiffTime -- ^ The time when the token was created from the epoch
            -> Integer -- ^ Number of sceonds the token is validity from creation time
            -> Value -- ^ The JSON value to use as an unregistered claim, the userid in this case
            -> Text  -- ^ The token
jsonToToken jwtSecret ndt len userId =
  encodeSigned (JWT.hmacSecret jwtSecret)
    mempty {typ = Just "JWT", alg = Just HS256}
    mempty {JWT.iat = numericDate ndt
           , JWT.exp = numericDate (ndt+fromIntegral len)
           , JWT.unregisteredClaims = ClaimsMap $ Map.fromList [(key, userId)]}

-- | Extract a JSON 'Value' out of a token
tokenToJson :: Text            -- ^ The secret to verify the signature with
            -> NominalDiffTime -- ^ The time compared with the expiration time for the token. Typically it is the current time.
            -> Text            -- ^ The token
            -> Maybe Value     -- ^ The JSON value, the userid in this case
tokenToJson jwtSecret now token = do
  jwt <- JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) token
  case hasDateExpired (JWT.exp (JWT.claims jwt)) (numericDate now) of
    Just False -> unClaimsMap (JWT.unregisteredClaims (JWT.claims jwt)) !? key
    _ -> Nothing

-- |Determines if a numeric date has expired
hasDateExpired :: Maybe JWT.NumericDate -- ^The expiration time
              -> Maybe JWT.NumericDate  -- ^The current time
              -> Maybe Bool             -- ^If the time has expired
hasDateExpired exptime currtime = (<) <$> exptime <*> currtime
