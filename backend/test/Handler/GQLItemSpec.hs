{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.GQLItemSpec (spec) where

import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Aeson (Value(..), encode)
import Database.Persist hiding (get)

import TestPlatform

import Yesod (liftIO)

import Accessability.Data.Item as DI
import Accessability.Data.Geo
import Accessability.Model.Database as DB
import Accessability.Model.REST.Authenticate
import Accessability.Model.REST.Item
import Accessability.Model.Transform
import Accessability.Utils.JWT
import Accessability.Settings

-- |Sets up the database for all the tests we run towards it
baseData::SqlPersistM ()
baseData = do
    now <- liftIO $ getCurrentTime
    _ <- insert $ DB.User "test" "$2b$10$Mrwvk0uFy/ZekcDeyGTyd.Lyx3k380XuB5zq1yaVxqEd6y5SxSrcG" "test@testland.com"
    _ <- insert $ DB.Item "Test-1" "GUID-1" "Test 1 Description" L1 Human Online Static Waiting (position 17.302273 62.393406) now
    _ <- insert $ DB.Item "Test-2" "GUID-2" "Test 2 Description" L2 Human Offline Static Approved (position 17.300922 62.393560) now
    _ <- insert $ DB.Item "Test-3" "GUID-3" "Test 3 Description" L3 Machine Offline Static Waiting (position 17.302562 62.393844) now
    _ <- insert $ DB.Item "Test-4" "GUID-4" "Test 4 Description" L4 Machine Offline Transient Approved (position 17.299657 62.393923) now
    _ <- insert $ DB.Item "Test-5" "GUID-5" "Test 5 Description" L5 Machine Online Transient Waiting (position 17.303989 62.393789) now
    _ <- insert $ DB.Item "Test-6" "GUID-6" "Test 6 Description" L5 Machine Online Transient Waiting (position (-17.303989) (-62.393789)) now
    _ <- insert $ DB.Item "Test-7" "GUID-7" "Test 7 Description" L5 Machine Online Transient Waiting (position (-17.303989) (-62.393789)) now
    _ <- insert $ DB.Item "Test-8" "GUID-8" "Test 8 Description" L5 Machine Online Transient Waiting (position (-17.303989) (-62.393789)) now
    return ()

-- |Our test item
newItem::IO PostItemBody
newItem = do
    now <- liftIO $ getCurrentTime
    return $ PostItemBody { postItemName = "test"
        , postItemGuid = "68436843-43ggfs-432gvvdd"
        , postItemDescription = "A test item"
        , postItemSource = Machine
        , postItemState = Online
        , postItemLevel = L1
        , postItemModifier = Static
        , postItemApproval = Waiting
        , postItemCreated = now
        , postItemLatitude = 62.393844
        , postItemLongitude = 17.302273}

-- |Our test item
updateItem::IO PutItemBody
updateItem = do
    now <- liftIO $ getCurrentTime
    return $ PutItemBody {
        putItemName = Just "Updated Name"
        , putItemGuid = Just "Updated GUID"
        , putItemDescription = Just "Updated Description"
        , putItemSource = Just Human
        , putItemState = Just Offline
        , putItemLevel = Just L1
        , putItemModifier = Just Static
        , putItemApproval = Just Approved
        , putItemCreated = Just now
        , putItemLatitude = Just 0
        , putItemLongitude = Just 0
        }

-- |Our base test query
queryItem::Float->IO PostItemsBody
queryItem d = do
    return PostItemsBody {
        postItemsLongitude = Just 17.302273
        , postItemsLatitude = Just 62.393844
        , postItemsDistance = Just d
        , postItemsLimit = Nothing
        , postItemsText = Nothing }

queryItemText::String->IO PostItemsBody
queryItemText s = do
    return PostItemsBody {
        postItemsLongitude = Nothing
        , postItemsLatitude = Nothing
        , postItemsDistance = Nothing
        , postItemsLimit = Nothing
        , postItemsText = Just $ pack s}

-- |The test specification
spec :: Spec
spec = withBaseDataAppOnce baseData $ do

        describe "Item handling for the GQL API" $ do

            it "Retrieve the GQL Schema" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Create an item
                pb <- liftIO $ newItem
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ "{\"query\":\"{__schema {types { name }}}\"}"
                (v::Maybe Value) <- getJsonBody
                liftIO $ putStrLn $ show v
                statusIs 200

