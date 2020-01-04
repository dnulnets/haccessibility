{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}

-- |
-- Module      : Accessability.Foundation
-- Description : The server foundation
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the foundation of the server and defines server and routes
-- types etc. to be able to run the application
--
module Accessability.Foundation (
    Server(..),
    Handler,
    Route (..),
    resourcesServer,
    getAuthenticatedUser,
    requireAuthentication) where

--
-- Standard libraries
--
import Data.Text (Text, pack)
import Data.Int (Int64)
import Data.Aeson (
  fromJSON,
  Result(..))
import Data.Time.Clock.System

--
-- Persistence libraries
--
import Database.Persist.Postgresql

--
-- Our own stuff
--
import Accessability.Settings (AppSettings(..))
import Accessability.Utils.JWT (tokenToJson)
import Accessability.Model.Transform (textToKey)

--
-- The HTTP server and network libraries
--
import Yesod
import Yesod.Auth
--import Yesod.Handler (permissionDenied)

--
-- Server session cookies
--
--import Web.ServerSession.Backend.Persistent
--import Web.ServerSession.Frontend.Yesod

-- | Our server and settings
data Server = Server {
    appSettings :: AppSettings             -- ^ Settings for the server
    , serverConnectionPool :: ConnectionPool -- ^ The pool of database connections
}

-- | The routes in our server
mkYesodData "Server" [parseRoutes|
/gql GQLR POST
/api/item CreateItemR POST
/api/item/#Text ItemR GET DELETE PUT
/api/items ItemsR POST
/api/authenticate AuthenticateR POST
|]

-- | Cookie name used for the sessions of this example app.
sessionCookieName :: Text
sessionCookieName = "IoTHub"

-- | Our server is a yesod instance, no session handling needed since
-- this is an API and we use JWT.
instance Yesod Server where

  -- We need no backend for this API-server
  makeSessionBackend _ = return Nothing

  -- We return with a simple text/plain error description
  errorHandler r = respond "text/plain" $ show r

  -- No middleware
  yesodMiddleware = id

-- | The persistence instance for the server
instance YesodPersist Server where

    -- | The persisten backend
    type YesodPersistBackend Server = SqlBackend

    -- | Executes the database action using the server database pool 
    runDB action = do
        server <- getYesod
        runSqlPool action $ serverConnectionPool server

--
-- Authorization interface. We do not use yesod standard interface since it is too much
-- focused on serving webpages and I could not really wrap my head around it for pure API
-- serving.
--

-- | Requires that the user is authenticated. if not it will short circuit the Handler and
-- return with a Permission Denied to the REST caller.
requireAuthentication :: Handler () -- ^ The Handler
requireAuthentication = do
  bearer <- lookupBearerAuth
  seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime    
  secret <- tokenSecret . appSettings <$> getYesod
  case bearer of
    Nothing -> do
      permissionDenied "You are not authenticated"
    Just token -> do
      case tokenToJson secret seconds token of
        Nothing -> do
          permissionDenied "You are not authenticated"
        Just _ -> do
          pure ()

-- | Checks to see if the caller is authenticated, if so it returns with the user identity
-- that was part of the JWT the caller sent with the request.
getAuthenticatedUser ::Handler (Maybe Text) -- ^ The user identity
getAuthenticatedUser = do
  bearer <- lookupBearerAuth
  seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime    
  secret <- tokenSecret . appSettings <$> getYesod
  return $ case bearer of
    Nothing -> Nothing
    Just token ->
      case tokenToJson secret seconds token of
        Nothing -> Nothing
        Just info ->
          case fromJSON info of
            Error _ -> Nothing
            Success uid -> Just $ uid
