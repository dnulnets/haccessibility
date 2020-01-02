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
import Data.Aeson (fromJSON,
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

  makeSessionBackend _ = return Nothing

  errorHandler r = respond "text/plain" $ show r

-- | The persistence instance for the server
instance YesodPersist Server where

    -- | The persisten backend
    type YesodPersistBackend Server = SqlBackend

    -- | Executes the database action using the server database pool 
    runDB action = do
        server <- getYesod
        runSqlPool action $ serverConnectionPool server

--
-- Authorization interface
--
requireAuthentication :: Handler ()
requireAuthentication = do
  bearer <- lookupBearerAuth
  liftIO $ print bearer
  seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime    
  secret <- tokenSecret . appSettings <$> getYesod
  case bearer of
    Nothing -> do
      liftIO $ print "Not authenticated"
      permissionDenied "You are not authenticated"
    Just token -> do
      liftIO $ print "Testing the token"
      case tokenToJson secret seconds token of
        Nothing -> do
          liftIO $ print "Not a valid token"
          permissionDenied "You are not authenticated"
        Just _ -> do
          liftIO $ print "Valid token, authenticated!"
          pure ()

getAuthenticatedUser ::Handler (Maybe Text)
getAuthenticatedUser = do
  bearer <- lookupBearerAuth
  liftIO $ print bearer
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

-- |Our application is a YesodAuth application
--instance YesodAuth Server where

    -- |Our authentication id
--    type AuthId Server = Text
  
    -- We are only publishing a REST JSON API, this is not needed but required
    -- by the Yesod API, implemented as error or empty
--    loginDest _ = error "This is an API, read the API documentation on authentication"
--    logoutDest _ = error "This is an API, read the API documentation on authentication"
--    authPlugins _ = []
--    authenticate _ = error "This is an API, read the API documentation on authentication"

    -- |Check the JSON Web token and return with the user identity if it is valid
--    maybeAuthId = do
--      bearer <- lookupBearerAuth
--      liftIO $ print bearer
--      seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime    
--      secret <- tokenSecret . appSettings <$> getYesod
--      return $ case bearer of
--        Nothing -> Nothing
--        Just token ->
--          case tokenToJson secret seconds token of
--            Nothing -> Nothing
--            Just info ->
--              case fromJSON info of
--                Error _ -> Nothing
--                Success uid -> Just $ uid

--
-- The rendermessage interface, needed by YesodAuth
--
--instance RenderMessage Server FormMessage where
--    renderMessage _ _ = defaultFormMessage