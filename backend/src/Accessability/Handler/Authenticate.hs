-- |
-- Module      : Heat.Handler.Authenticate
-- Description : The handler for the authenticate route
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the authenticate route for the application.
--
module Accessability.Handler.Authenticate (postAuthenticateR) where

--
-- External imports
--
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.HexString (HexString)
import Data.ByteString (ByteString)
import Data.Time.Clock.System (
  getSystemTime,
  SystemTime(..))

import Network.HTTP.Types.Status (status401)

import Database.Persist.Sql

import Yesod

--
-- Internal imports
--
import Accessability.Model.Database
import Accessability.Model.Transform (keyToText)
import Accessability.Foundation (Server(..), Handler)
import Accessability.Utils.JWT (jsonToToken)
import Accessability.Utils.Password (authHashPassword, authValidatePassword)
import Accessability.Interface.Authenticate (Authenticate(..), UserInfo (..))
import Accessability.Settings (AppSettings(..))

-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
postAuthenticateR :: Handler Value -- ^ The logged in user and the token
postAuthenticateR = do
  auth <- requireCheckJsonBody :: Handler Authenticate
  dbuser <- runDB $ getBy $ UniqueUserUsername $ username auth
  seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
  appset <- appSettings <$> getYesod
  let secret = tokenSecret appset
      length = tokenExpiration appset
    in case dbuser of
         Just (Entity userId user) | authValidatePassword (userPassword user) (password auth) -> do
                                       token <- return $ jsonToToken secret seconds length $ toJSON $ keyToText userId
                                       returnJson $ UserInfo (keyToText userId) token (userUsername user) (userEmail user)
         _ -> sendResponseStatus status401 Null
