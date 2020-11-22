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
module Accessability.Handler.REST.Authenticate (postAuthenticateR, getAuthenticateR) where

--
-- External imports
--
import           Data.Time.Clock.System                (SystemTime (..),
                                                        getSystemTime)

import           Network.HTTP.Types.Status             (status401)

import Database.Persist.Sql
    ( Entity(Entity), PersistStoreRead(get), PersistUniqueRead(getBy) )

import Yesod
    ( Value(Null),
      ToJSON(toJSON),
      MonadIO(liftIO),
      getYesod,
      lookupBearerAuth,
      sendResponseStatus,
      requireCheckJsonBody,
      returnJson,
      YesodPersist(runDB) )

--
-- Internal imports
--
import           Accessability.Foundation              (Handler, Server (..),
                                                        getAuthenticatedUserInfo)
import Accessability.Model.Database
    ( Unique(UniqueUserUsername),
      User(userEmail, userUsername, userPassword, userRole) )
import           Accessability.Model.REST.Authenticate (Authenticate (..),
                                                        UserInfo (..),
                                                        TokenInfo(..))
import           Accessability.Model.Transform         (keyToText, textToKey)
import           Accessability.Settings                (AppSettings (..))
import           Accessability.Utils.JWT               (jsonToToken)
import           Accessability.Utils.Password          (authValidatePassword)

-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
postAuthenticateR :: Handler Value -- ^ The logged in user and the token
postAuthenticateR = do
  auth <- requireCheckJsonBody :: Handler Authenticate
  dbuser <- runDB $ getBy $ UniqueUserUsername $ username auth
  seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
  appset <- appSettings <$> getYesod
  let secret = tokenSecret appset
      len = tokenExpiration appset
    in case dbuser of
         Just (Entity userId user) | authValidatePassword (userPassword user) (password auth) -> do
                                       let token = jsonToToken secret seconds len $ toJSON $ TokenInfo (keyToText userId) (userRole user)
                                       returnJson $ UserInfo (keyToText userId) token (userUsername user) (userRole user) (userEmail user)
         _ -> sendResponseStatus status401 Null

-- |Get the authentication bearer so we can extract the userid out of it and search for the
-- user and return with it if it exists
getAuthenticateR :: Handler Value
getAuthenticateR = do
  mui <- getAuthenticatedUserInfo
  case mui of
    Nothing ->
      sendResponseStatus status401 Null
    Just ui -> do
      token <- lookupBearerAuth
      case token of
        Just k -> do
          dbuser <- runDB $ get $ textToKey $ tiuserid ui
          case dbuser of
            Just user ->
              returnJson $ UserInfo (tiuserid ui) k (userUsername user) (userRole user) (userEmail user)
            _ ->
              sendResponseStatus status401 Null
        _ ->
          sendResponseStatus status401 Null
