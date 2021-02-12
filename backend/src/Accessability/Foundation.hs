{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wwarn=unused-top-binds #-}
{-# OPTIONS_GHC -fforce-recomp #-}

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
    getAuthenticatedUserInfo,
    requireAuthentication,
    requireAuthenticationAndRole) where

--
-- Standard libraries
--
import           Data.Aeson                  (Result (..), fromJSON)
import           Data.Text                   (Text)
import           Data.Time.Clock.System

--
-- Persistence libraries
--
import           Database.Persist.Postgresql

--
-- Network
--
import Network.HTTP.Types (status403)

--
-- Our own stuff
--
import           Accessability.Settings      (AppSettings (..))
import           Accessability.Utils.JWT     (tokenToJson)
import           Accessability.Model.REST.Authenticate (TokenInfo(..))
import           Accessability.Data.User (Role)

--
-- The HTTP server and network libraries
--
import           Yesod
import           Yesod.Auth
import           Yesod.Static

-- | Our server and settings
data Server = Server {
    getStatic              :: Static,                   -- ^ All static files
    appSettings            :: AppSettings             -- ^ Settings for the server
    , serverConnectionPool :: ConnectionPool -- ^ The pool of database connections
}

-- | Our static files
publicFiles "static"

-- | The routes in our server
mkYesodData "Server" [parseRoutes|
/iothub/gql GQLR POST
/iothub/api/item CreateItemR POST
/iothub/api/item/#Text ItemR GET DELETE PUT
/iothub/api/item/#Text/attributes ItemAttributesR GET PUT
/iothub/api/items ItemsR POST
/iothub/api/itemsandvalues ItemsAndValuesR POST
/iothub/api/user/properties UserPropertiesR GET PUT
/iothub/api/authenticate AuthenticateR POST GET
/iothub/api/attributes AttributesR GET
!/iothub/ StaticR Static getStatic
|]

-- | Our server is a yesod instance, no session handling needed since
-- this is an API and we use JWT.
instance Yesod Server where

  -- We need no backend for this API-server
  makeSessionBackend _ = return Nothing

  -- We return with a simple text/plain error description
  errorHandler r = respond "text/plain" $ show r

  -- No middleware
  yesodMiddleware handler = do
    addHeader "Cache-Control" "no-cache, must-revalidate"
    defaultYesodMiddleware handler


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
  userInfo <- getAuthenticatedUserInfo
  case userInfo of
    Nothing -> sendStatusJSON status403 ()
    Just _  -> pure ()

-- | Requires that the user is authenticated. if not it will short circuit the Handler and
-- return with a Permission Denied to the REST caller.
requireAuthenticationAndRole :: Role->Handler () -- ^ The Handler
requireAuthenticationAndRole r = do
  mti <- getAuthenticatedUserInfo
  case mti of
    Nothing -> sendStatusJSON status403 ()
    Just (TokenInfo uid role)  -> if role == r then pure () else sendStatusJSON status403 ()

-- | Checks to see if the caller is authenticated, if so it returns with the user identity
-- that was part of the JWT the caller sent with the request.
getAuthenticatedUserInfo ::Handler (Maybe TokenInfo) -- ^ The user identity
getAuthenticatedUserInfo = do
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
            Error _     -> Nothing
            Success ui -> Just ui
