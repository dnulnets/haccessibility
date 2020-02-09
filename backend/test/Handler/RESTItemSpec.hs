{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.RESTItemSpec (spec) where

import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
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
        , postItemLatitude = 10.1
        , postItemLongitude = -30.2}

-- |Our base test query
queryItem::IO PostItemsBody
queryItem = do
    return PostItemsBody {
        postItemsLongitude = Just 62.393844
        , postItemsLatitude = Just 17.302273
        , postItemsDistance = Just 1.0
        , postItemsLimit = Nothing
        , postItemsText = Nothing }

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

                -- Request the same item with the item id
                request $ do
                    setMethod "GET"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl $ ItemR $ fromMaybe "" (DI.itemId ri)
                    setRequestBody $ encode pb
                (rj::DI.Item) <- getJsonBody              
                statusIs 200

                -- Check that returned item is the same as we sent in
                liftIO $ postItemName pb `shouldBe` DI.itemName rj
                liftIO $ postItemGuid pb `shouldBe` DI.itemGuid rj
                liftIO $ postItemDescription pb `shouldBe` DI.itemDescription rj
                liftIO $ postItemSource pb `shouldBe` DI.itemSource rj
                liftIO $ postItemState pb `shouldBe` DI.itemState rj
                liftIO $ postItemLevel pb `shouldBe` DI.itemLevel rj
                liftIO $ postItemModifier pb `shouldBe` DI.itemModifier rj
                liftIO $ postItemApproval pb `shouldBe` DI.itemApproval rj
                liftIO $ diffUTCTime (postItemCreated pb) (DI.itemCreated rj) `shouldSatisfy` timeProximity
                liftIO $ (postItemLatitude pb)-(DI.itemLatitude rj) `shouldSatisfy` lolaProximity
                liftIO $ (postItemLongitude pb)-(DI.itemLongitude rj) `shouldSatisfy` lolaProximity

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
                q <- liftIO $ queryItem
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q                
                (ai::[DI.Item]) <- getJsonBody              
                statusIs 200
