{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Module      : Acessability.Handler.REST
-- Description : The REST API entrypoint
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the handler for REST API
--
module Accessability.Handler.REST.Item
    ( getItemR
    , getItemAndValuesR
    , putItemR
    , deleteItemR
    , postCreateItemR
    , postItemsR
    , getAttributesR
    , getItemAttributesR
    , putItemAttributesR
    , postItemsAndValuesR
    )
where

--
-- Import standard libs
--
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text, pack, splitOn)
import qualified UnliftIO.Exception             as UIOE

--
-- Yesod and HTTP imports
--
import           Network.HTTP.Types             (status200)
import           Yesod

--
-- My own imports
--
import           Accessability.Data.Analysis    (evaluatePOI)
import           Accessability.Data.Functor
import           Accessability.Data.Geo
import           Accessability.Data.User (Role(..))
import           Accessability.Data.Item        (Attribute (..), Item (..),
                                                 ItemValue (..))
import           Accessability.Foundation       (Handler, getAuthenticatedUserInfo,
                                                 requireAuthentication, requireAuthenticationAndRole)
import qualified Accessability.Handler.Database as DBF
import qualified Accessability.Model.Database   as DB
import           Accessability.Model.REST.Item
import           Accessability.Model.Transform
import           Accessability.Model.REST.Authenticate(TokenInfo(..))

-- | The REST GET handler for an item, i.e. return with the data of an item based on the items
-- key provided in the URL api/item/0000000000000001
--
getItemR:: Text      -- ^ The item key
  -> Handler Value -- ^ The item as a JSON response
getItemR key = do
    requireAuthentication
    result <- UIOE.catchAny
        (fffmap toGenericItem $ DBF.dbFetchItem $ textToKey key)
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to get the item from the database", key]
                <> splitOn "\n" (pack e)
        Right Nothing  -> sendResponseNoContent
        Right (Just i) -> sendStatusJSON status200 i

-- | The REST GET handler for an item, i.e. return with the data of an item based on the items
-- key provided in the URL api/item/0000000000000001
--
getItemAndValuesR:: Text      -- ^ The item key
  -> Handler Value -- ^ The item as a JSON response
getItemAndValuesR key = do
    requireAuthentication

    -- Get the user properties
    mui <- getAuthenticatedUserInfo    
    props <- case mui of
        Just ui -> do
            result <- UIOE.catchAny
                (fffmap toGenericUserProperty $ DBF.dbFetchUserProperties $ textToKey $ tiuserid ui)
                (pure . Left . show)
            case result of
                Left _  -> pure []
                Right a -> pure a
        Nothing -> pure []

    result <- UIOE.catchAny
        (fffmap toGenericItem $ DBF.dbFetchItem $ textToKey key)
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to get the item from the database", key]
                <> splitOn "\n" (pack e)
        Right Nothing  -> sendResponseNoContent
        Right (Just i) -> do

            -- Calculate the value of the POI in respect to the user properties
            attrs <- fetchItemAttributes $ itemId i

            -- Send it back
            sendStatusJSON status200 $ mergeItem i $ evaluatePOI props attrs

-- | Merge an item with its values
mergeItem:: Item        -- ^ The item
         -> ItemValue   -- ^ The items accessibility values
         ->Item
mergeItem item iv = item {itemPositive = Just $ positive iv
                        , itemNegative = Just $ negative iv
                        , itemUnknown = Just $ unknown iv
                        , itemPositiveAttributes = Just $ positiveAttributes iv
                        , itemNegativeAttributes = Just $ negativeAttributes iv
                        , itemUnknownAttributes = Just $ unknownAttributes iv}

-- | Fetch all attributes for an item
fetchItemAttributes::Maybe Text->Handler [Attribute]
fetchItemAttributes Nothing = pure []
fetchItemAttributes (Just key) = do
    result <- UIOE.catchAny
        (fffmap toGenericItemAttribute $ DBF.dbFetchItemAttributes $ textToKey key)
        (pure . Left . show)
    case result of
        Left _  -> pure []
        Right a -> pure a

-- | The REST delete handler, i.e. return with the data of an item based on the items
-- key and delete the item.
deleteItemR:: Text      -- ^ The item key
            -> Handler () -- ^ The item as a JSON response
deleteItemR key = do
    requireAuthenticationAndRole Administrator
    result <- UIOE.catchAny (DBF.dbDeleteItem $ textToKey key)
                            (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to delete the item from the database", key]
                <> splitOn "\n" (pack e)
        Right _ -> sendResponseNoContent

-- | The REST put handler, i.e. return with the updated data of the changed item based
-- on the specified key
putItemR
    :: Text      -- ^ The item key
    -> Handler Value -- ^ The item as a JSON response
putItemR key = do
    requireAuthenticationAndRole Administrator
    queryBody <- requireCheckJsonBody :: Handler PutItemBody
    result    <- UIOE.catchAny
        (  fffmap toGenericItem
        $  DBF.dbUpdateItem (textToKey key)
        $  DBF.changeField DB.ItemName (putItemName queryBody)
        <> DBF.changeField DB.ItemGuid (putItemGuid queryBody)
        <> DBF.changeField DB.ItemDescription (putItemDescription queryBody)
        <> DBF.changeField DB.ItemSource (putItemSource queryBody)
        <> DBF.changeField DB.ItemModifier (putItemModifier queryBody)
        <> DBF.changeField DB.ItemApproval (putItemApproval queryBody)
        <> DBF.changeField
               DB.ItemPosition
               (maybePosition (realToFrac <$> putItemLongitude queryBody)
                              (realToFrac <$> putItemLatitude queryBody)
               )
        )
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to update the item in the database", key]
                <> splitOn "\n" (pack e)
        Right item -> sendStatusJSON status200 item

-- | The REST post handler, i.e. creates a new item with the specified data in the body
-- and return with the data as stored in the database.
postCreateItemR :: Handler Value -- ^ The item as a JSON response
postCreateItemR = do
    requireAuthenticationAndRole Administrator
    body   <- requireCheckJsonBody :: Handler PostItemBody
    result <- UIOE.catchAny
        (fffmap toGenericItem DBF.dbCreateItem $ DB.Item
            { DB.itemGuid        = postItemGuid body
            , DB.itemCreated     = postItemCreated body
            , DB.itemModifier    = postItemModifier body
            , DB.itemApproval    = postItemApproval body
            , DB.itemName        = postItemName body
            , DB.itemDescription = postItemDescription body
            , DB.itemSource      = postItemSource body
            , DB.itemPosition    = Position $ PointXY
                                       (realToFrac $ postItemLongitude body)
                                       (realToFrac $ postItemLatitude body)
            }
        )
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to create a new item in the database"]
                <> splitOn "\n" (pack e)
        Right item -> sendStatusJSON status200 item

-- | The REST get handler for items, i.e. a list of items based on a body where the
-- search fields are spceified.
postItemsR :: Handler Value    -- ^ The list of items as a JSON response
postItemsR = do
    requireAuthentication
    queryBody <- requireCheckJsonBody :: Handler PostItemsBody
    result    <- UIOE.catchAny
        (fffmap
            toGenericItem
            (DBF.dbFetchItems
                (postItemsText queryBody)
                (maybePosition (realToFrac <$> postItemsLongitude queryBody)
                               (realToFrac <$> postItemsLatitude queryBody)
                )
                (realToFrac <$> postItemsDistance queryBody)
                (postItemsLimit queryBody)
            )
        )
        (pure . Left . show)
    case result of
        Left e -> do
            liftIO $ putStrLn "Unable to find any items"
            invalidArgs
                $  ["Unable to find any items in the database"]
                <> splitOn "\n" (pack e)
        Right items ->
            sendStatusJSON status200 items

-- | The REST get handler for items, i.e. a list of items based on a body where the
-- search fields are spceified.
postItemsAndValuesR :: Handler Value    -- ^ The list of items as a JSON response
postItemsAndValuesR = do
    requireAuthentication

    -- Get the user properties
    mui <- getAuthenticatedUserInfo
    props <- case mui of
        Just ui -> do
            result <- UIOE.catchAny
                (fffmap toGenericUserProperty $ DBF.dbFetchUserProperties $ textToKey $ tiuserid ui)
                (pure . Left . show)
            case result of
                Left _  -> pure []
                Right a -> pure a
        Nothing -> pure []

    -- Get the items
    queryBody <- requireCheckJsonBody :: Handler PostItemsBody
    items    <- either (const []) id <$> UIOE.catchAny
        (fffmap
            toGenericItem
            (DBF.dbFetchItems
                (postItemsText queryBody)
                (maybePosition (realToFrac <$> postItemsLongitude queryBody)
                               (realToFrac <$> postItemsLatitude queryBody)
                )
                (realToFrac <$> postItemsDistance queryBody)
                (postItemsLimit queryBody)
            )
        )
        (pure . Left . show)

    -- Calculate the value of the POI in respect to the user properties
    attrs <- sequence $ fetchItemAttributes <$> (itemId <$> items)
    sendStatusJSON status200 $ zipWith mergeItem items $ evaluatePOI props <$> attrs

-- | The REST get handler for attributes, i.e. a list of attributes that an item can
-- have.
getAttributesR :: Handler Value    -- ^ The list of items as a JSON response
getAttributesR = do
    requireAuthentication
    result <- UIOE.catchAny
        (fffmap toGenericAttribute DBF.dbFetchAttributes)
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to get the attributes from the database"]
                <> splitOn "\n" (pack e)
        Right a -> sendStatusJSON status200 a

-- | The REST GET handler for an item, i.e. return with the data of an item based on the items
-- key provided in the URL api/item/0000000000000001
getItemAttributesR
    :: Text      -- ^ The item key
    -> Handler Value -- ^ The list of possible attributes and their values, if any
getItemAttributesR key = do
    requireAuthentication
    result <- UIOE.catchAny
        (fffmap toGenericItemAttribute $ DBF.dbFetchItemAttributes $ textToKey
            key
        )
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to get the item from the database", key]
                <> splitOn "\n" (pack e)
        Right a -> sendStatusJSON status200 a


-- | The REST PUT handler for the attributes of an item
--
-- If the record has an attributeValueID and a value it is an update
-- If the record has an attributeValueID and no value it is a delete
-- If the record has no attributeValueID and a value it is an insert
-- If the record has no attributeValueID and no value it is ignored
putItemAttributesR :: Text -> Handler Value
putItemAttributesR key = do
    requireAuthenticationAndRole Administrator
    queryBody <- requireCheckJsonBody :: Handler [PutItemAttributes]
    result    <- UIOE.catchAny
        (DBF.dbUpdateItemAttributes (doit <$> queryBody))
        (pure . Left . show)
    case result of
        Left e ->
            invalidArgs
                $  ["Unable to update the items parameters ", key]
                <> splitOn "\n" (pack e)
        Right _ -> sendResponseStatus status200 Null
  where
    doit
        :: PutItemAttributes
        -> (Maybe (Key DB.AttributeValue), Maybe DB.AttributeValue)
    doit pia =
        ( textToKey <$> putItemAttributesAttributeValueId pia
        , case putItemAttributesValue pia of
            Just v -> Just $ DB.AttributeValue
                { DB.attributeValueAttribute = textToKey $ fromMaybe
                                                   "0000000000000000"
                                                   (putItemAttributesAttributeId
                                                       pia
                                                   )
                , DB.attributeValueItem      = textToKey key
                , DB.attributeValueValue     = v
                }
            Nothing -> Nothing
        )
