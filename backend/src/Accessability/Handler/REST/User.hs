{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Acessability.Handler.REST.User
-- Description : The REST API entrypoint for the User
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the handler for REST API
--
module Accessability.Handler.REST.User (getUserPropertiesR, putUserPropertiesR) where

--
-- Import standard libs
--
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text, pack, splitOn)
import qualified UnliftIO.Exception             as UIOE

--
-- Yesod and HTTP imports
--
import           Network.HTTP.Types             (status200, status401)
import           Yesod

--
-- My own imports
--
import           Accessability.Data.Functor
import           Accessability.Foundation       (Handler, getAuthenticatedUserInfo)
import qualified Accessability.Handler.Database as DBF
import qualified Accessability.Model.Database   as DB
import           Accessability.Model.REST.User
import           Accessability.Model.Transform
import Accessability.Model.REST.Authenticate
    ( TokenInfo(tiuserid) )

-- | The REST GET handler for an item, i.e. return with the data of an item based on the items
-- key provided in the URL api/item/0000000000000001
getUserPropertiesR :: Handler Value -- ^ The list of possible attributes and their values, if any
getUserPropertiesR = do
    mui <- getAuthenticatedUserInfo
    case mui of
        Just ui -> do
            result <- UIOE.catchAny
                (fffmap toGenericUserProperty $ DBF.dbFetchUserProperties $ textToKey $ tiuserid ui)
                (pure . Left . show)
            case result of
                Left e ->
                    invalidArgs
                        $  ["Unable to get the item from the database", tiuserid ui]
                        <> splitOn "\n" (pack e)
                Right a -> sendStatusJSON status200 a
        Nothing ->
            sendResponseStatus status401 Null

-- | The REST PUT handler for the attributes of an item
--
-- If the record has an attributeValueID and an oeration it is an update
-- If the record has an attributeValueID and no operation it is a delete
-- If the record has no attributeValueID and an operation it is an insert
-- If the record has no attributeValueID and no operation it is ignored
putUserPropertiesR :: Handler Value
putUserPropertiesR = do
    mui <- getAuthenticatedUserInfo
    case mui of
        Just ui -> do
            queryBody <- requireCheckJsonBody :: Handler [PutUserProperty]
            liftIO $ print queryBody
            result    <- UIOE.catchAny
                (DBF.dbUpdateUserProperties (doit (tiuserid ui) <$> queryBody))
                (pure . Left . show)
            case result of
                Left e ->
                    invalidArgs
                        $  ["Unable to update the items parameters ", tiuserid ui]
                        <> splitOn "\n" (pack e)
                Right _ -> sendResponseStatus status200 Null
        Nothing ->
            sendResponseStatus status401 Null

  where
    doit
        :: Text -> PutUserProperty
        -> (Maybe (Key DB.UserProperty), Maybe DB.UserProperty)
    doit key pia =
        ( textToKey <$> upuserPropertyId pia
        , case upoperation pia of
            Just v -> Just $ DB.UserProperty
                { DB.userPropertyAttribute = textToKey $ fromMaybe
                                                   "0000000000000000"
                                                   (upattributeId pia)
                , DB.userPropertyNegate = Just True == upnegate pia
                , DB.userPropertyValue = fromMaybe "" $ upvalue pia
                , DB.userPropertyValue1 = upvalue1 pia
                , DB.userPropertyOperation     = v
                , DB.userPropertyUser = textToKey key
                }
            Nothing -> Nothing
        )
