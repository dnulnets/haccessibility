{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.RESTItemSpec (spec) where

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

        describe "Item handling for the REST API" $ do

            it "Create and retrieve the same item" $ do

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
                    setUrl CreateItemR
                    setRequestBody $ encode pb
                (ri::DI.Item) <- getJsonBody              
                statusIs 200

                -- Check that returned item is the same as we sent in
                liftIO $ postItemName pb `shouldBe` DI.itemName ri
                liftIO $ postItemGuid pb `shouldBe` DI.itemGuid ri
                liftIO $ postItemDescription pb `shouldBe` DI.itemDescription ri
                liftIO $ postItemSource pb `shouldBe` DI.itemSource ri
                liftIO $ postItemState pb `shouldBe` DI.itemState ri
                liftIO $ postItemLevel pb `shouldBe` DI.itemLevel ri
                liftIO $ postItemModifier pb `shouldBe` DI.itemModifier ri
                liftIO $ postItemApproval pb `shouldBe` DI.itemApproval ri
                liftIO $ diffUTCTime (postItemCreated pb) (DI.itemCreated ri) `shouldSatisfy` timeProximity
                liftIO $ (postItemLatitude pb)-(DI.itemLatitude ri) `shouldSatisfy` lolaProximity
                liftIO $ (postItemLongitude pb)-(DI.itemLongitude ri) `shouldSatisfy` lolaProximity

            it "Query items within distance 1m, should only one" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Query for items within one meter
                q <- liftIO $ queryItem 1
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q                
                (ai::[DI.Item]) <- getJsonBody
                liftIO $ length ai `shouldBe` 1            
                statusIs 200

            it "Query items within distance 80m, should only be four" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Query for items within 80 meter
                q <- liftIO $ queryItem 80
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q                
                (ai::[DI.Item]) <- getJsonBody
                statusIs 200

            it "Update item" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Query for the item to update TEST-6
                q <- liftIO $ queryItemText "TEST-6"
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q                
                (ai::[DI.Item]) <- getJsonBody
                liftIO $ length ai `shouldBe` 1

                -- Update the item
                u <- liftIO updateItem
                request $ do
                    setMethod "PUT"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl $ ItemR $ fromMaybe "" (DI.itemId (ai !! 0))
                    setRequestBody $ encode u
                (ri::DI.Item) <- getJsonBody
                statusIs 200

                -- Check that returned item is the same as we sent in
                liftIO $ fromJust (putItemName u) `shouldBe` DI.itemName ri
                liftIO $ fromJust (putItemGuid u) `shouldBe` DI.itemGuid ri
                liftIO $ fromJust (putItemDescription u) `shouldBe` DI.itemDescription ri
                liftIO $ fromJust (putItemSource u) `shouldBe` DI.itemSource ri
                liftIO $ fromJust (putItemState u) `shouldBe` DI.itemState ri
                liftIO $ fromJust (putItemLevel u) `shouldBe` DI.itemLevel ri
                liftIO $ fromJust (putItemModifier u) `shouldBe` DI.itemModifier ri
                liftIO $ fromJust (putItemApproval u) `shouldBe` DI.itemApproval ri
                liftIO $ diffUTCTime (fromJust $ putItemCreated u) (DI.itemCreated ri) `shouldSatisfy` timeProximity
                liftIO $ (fromJust $ putItemLatitude u)-(DI.itemLatitude ri) `shouldSatisfy` lolaProximity
                liftIO $ (fromJust $ putItemLongitude u)-(DI.itemLongitude ri) `shouldSatisfy` lolaProximity

            it "Get item" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Query for the item to update TEST-7
                q <- liftIO $ queryItemText "TEST-7"
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q                
                (ai::[DI.Item]) <- getJsonBody
                liftIO $ length ai `shouldBe` 1

                -- Fetch the item
                request $ do
                    setMethod "GET"
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl $ ItemR $ fromJust (DI.itemId (ai !! 0))
                (ri::DI.Item) <- getJsonBody
                statusIs 200

                -- Check that returned item is the right item
                liftIO $ "Test-7" `shouldBe` DI.itemName ri
                liftIO $ "GUID-7" `shouldBe` DI.itemGuid ri
                liftIO $ "Test 7 Description" `shouldBe` DI.itemDescription ri
                liftIO $ Machine `shouldBe` DI.itemSource ri
                liftIO $ Online `shouldBe` DI.itemState ri
                liftIO $ L5 `shouldBe` DI.itemLevel ri
                liftIO $ Transient `shouldBe` DI.itemModifier ri
                liftIO $ Waiting `shouldBe` DI.itemApproval ri
                liftIO $ (-62.393789)-(DI.itemLatitude ri) `shouldSatisfy` lolaProximity
                liftIO $ (-17.303989)-(DI.itemLongitude ri) `shouldSatisfy` lolaProximity

            it "Delete item" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Query for the item to update TEST-8
                q <- liftIO $ queryItemText "TEST-8"
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q                
                (ai::[DI.Item]) <- getJsonBody
                liftIO $ length ai `shouldBe` 1

                -- Delete the item
                request $ do
                    setMethod "DELETE"
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl $ ItemR $ fromJust (DI.itemId (ai !! 0))
                statusIs 204
                
                -- Make sure it does not work to fetch it, it has been deleted
                request $ do
                    setMethod "GET"
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl $ ItemR $ fromJust (DI.itemId (ai !! 0))
                statusIs 204
                
