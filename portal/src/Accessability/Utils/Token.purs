-- |
-- | The utility module for token handling in local storage
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Utils.Token (Token
                        , readToken
                        , writeToken
                        , removeToken) where

-- Language imports
import Prelude

import Data.Maybe (Maybe)

import Effect (Effect)

import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- | A type for the token
newtype Token = Token String

derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Token _) = "Token {- secret -}"

-- | The name of the localstorage key
tokenKey = "token" :: String

-- | Read the token from local storage
readToken :: Effect (Maybe Token) -- ^The token
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

-- | Write the token to local storage
writeToken :: Token        -- ^The token to store
           -> Effect Unit  -- ^Unit return
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

-- | Remove the token from local storage
removeToken :: Effect Unit -- ^Unit return
removeToken =
  removeItem tokenKey =<< localStorage =<< window
