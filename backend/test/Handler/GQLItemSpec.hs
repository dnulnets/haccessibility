{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.GQLItemSpec (spec) where

import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text,pack)
import Data.Set (Set, isSubsetOf, fromList)
import Data.Time.Clock
import Data.Time.Clock.System

import Data.Aeson (Value(..), Object (..), (.:), encode,
    FromJSON, Options(..), defaultOptions)
import Data.Aeson.Types (parse, Result(..), Parser)
import Data.Aeson.TH (deriveJSON)

import Database.Persist hiding (get)

import TestPlatform

import Yesod (liftIO)

import           Data.Morpheus.Types          (ID (..), unpackID)

import Accessability.Data.Item as DI
import Accessability.Data.Geo
import Accessability.Data.Functor
import Accessability.Model.Database as DB
import Accessability.Model.GQL as GQL
import Accessability.Model.REST.Authenticate
import Accessability.Model.REST.Item
import Accessability.Model.Transform
import Accessability.Utils.JWT
import Accessability.Settings
import Accessability.Utils.JSON (firstLower)

-- | Test types
data TestItem = TestItem {
    testItemId            :: Maybe Text  -- ^ The ID of the item
    , testItemName        :: Text  -- ^ The name of the item
    , testItemGuid        :: Text  -- ^ The external unique identifier of the item
    , testItemDescription :: Text       -- ^ The description of the item
    , testItemSource      :: ItemSource      -- ^ How the items online state is determined
    , testItemModifier    :: ItemModifier     -- ^ The modifier of the item
    , testItemApproval    :: ItemApproval     -- ^ The approval state of the item
    , testItemLatitude    :: Float        -- ^ The latitude of the item
    , testItemLongitude   :: Float       -- ^ The longitude of the item
    , testItemCreated     :: UTCTime   -- ^ The zoned time of the item
    , testItemDistance    :: Maybe Float -- ^ The distance from a specified point provided by the query
    }
    -- deriving (Generic)

-- |Automatically derive JSON of TestItem
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'test' in the field names
  } ''TestItem)


-- |Sets up the database for all the tests we run towards it
baseData::SqlPersistM ()
baseData = do
    now <- liftIO $ getCurrentTime
    _ <- insert $ DB.User "test" "$2b$10$Mrwvk0uFy/ZekcDeyGTyd.Lyx3k380XuB5zq1yaVxqEd6y5SxSrcG" "test@testland.com"
    _ <- insert $ DB.Item "Test-1" "GUID-1" "Test 1 Description" Human Static Waiting (position 17.302273 62.393406) now
    _ <- insert $ DB.Item "Test-2" "GUID-2" "Test 2 Description" Human Static Approved (position 17.300922 62.393560) now
    _ <- insert $ DB.Item "Test-3" "GUID-3" "Test 3 Description" Machine Static Waiting (position 17.302562 62.393844) now
    _ <- insert $ DB.Item "Test-4" "GUID-4" "Test 4 Description" Machine Transient Approved (position 17.299657 62.393923) now
    _ <- insert $ DB.Item "Test-5" "GUID-5" "Test 5 Description" Machine Transient Waiting (position 17.303989 62.393789) now
    _ <- insert $ DB.Item "Test-6" "GUID-6" "Test 6 Description" Machine Transient Waiting (position (-17.303989) (-62.393789)) now
    _ <- insert $ DB.Item "Test-7" "GUID-7" "Test 7 Description" Machine Transient Waiting (position (-17.303989) (-62.393789)) now
    _ <- insert $ DB.Item "Test-8" "GUID-8" "Test 8 Description" Machine Transient Waiting (position (-17.303989) (-62.393789)) now
    return ()

-- |The set of identifiers expected in the graphQL schema
gqlItems::Set String
gqlItems = fromList ["ItemApproval", "ItemSource", "UTCTime", "ItemModifier", "Item", "Query", "Mutation"]

-- |graphql get item query
gqlQueryItem::Text->Text
gqlQueryItem id = "query FetchThemAll { queryItem (queryItemId: \\\"" <> id <>
    "\\\") {itemId itemName itemGuid itemDescription itemSource itemModifier itemApproval itemLatitude itemLongitude itemDistance itemCreated}}"

-- |graphql get items query
gqlQueryItems::Text
gqlQueryItems = "query FetchThemAll {queryItems (queryItemsText: \\\"%\\\") {itemId}}"

-- |graphql get items query
gqlQueryItemsD::Text->Text
gqlQueryItemsD d = "query FetchThemAll {queryItems (queryItemsDistance: " <> d
    <> ", queryItemsLongitude: 17.302273, queryItemsLatitude: 62.393406) {itemId}}"

queryItemText::String->IO PostItemsBody
queryItemText s = do
    return PostItemsBody {
        postItemsLongitude = Nothing
        , postItemsLatitude = Nothing
        , postItemsDistance = Nothing
        , postItemsLimit = Nothing
        , postItemsText = Just $ pack s}

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

                -- Retrieve the schema
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ "{\"query\":\"{__schema {types { name }}}\"}"
                statusIs 200

                -- Check the result
                (v::Object) <- getJsonBody
                liftIO $ case parse locateSchema v of 
                    Error s -> expectationFailure s
                    Success a -> (gqlItems `isSubsetOf` (fromList a)) `shouldBe` True

            it "Fetch an item using queryItem" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Query for the item to read TEST-1
                q <- liftIO $ queryItemText "TEST-1"
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl ItemsR
                    setRequestBody $ encode q
                let clean i = fromJust $ DI.itemId i                
                (ai::[Text]) <- ffmap clean getJsonBody
                liftIO $ length ai `shouldBe` 1

                -- Retrieve the item using GQL
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ fromStrict . encodeUtf8 $ "{\"query\":\"" <> (gqlQueryItem (ai !! 0)) <> 
                        "\", \"operationName\":\"FetchThemAll\"}"                    
                statusIs 200

                (v::Object) <- getJsonBody
                liftIO $ case parse (locateItem "queryItem") v of 
                    Error s -> expectationFailure s
                    Success a -> do
                        now <- getCurrentTime
                        (fromJust $ testItemId a) `shouldBe` (ai !! 0)
                        (testItemName a) `shouldBe` "Test-1"
                        (testItemGuid a) `shouldBe` "GUID-1"
                        (testItemDescription a) `shouldBe` "Test 1 Description"
                        (testItemSource a) `shouldBe` DI.Human
                        (testItemModifier a) `shouldBe` DI.Static
                        (testItemApproval a) `shouldBe` DI.Waiting
                        ((testItemLongitude a) - 17.302273) `shouldSatisfy` lolaProximity
                        ((testItemLatitude a) - 62.393406) `shouldSatisfy` lolaProximity
                        diffUTCTime (testItemCreated a) now `shouldSatisfy` timeProximity

            it "Query all items, should be 8" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Retrieve the items using GQL
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ fromStrict . encodeUtf8 $ "{\"query\":\"" <> gqlQueryItems <> 
                        "\", \"operationName\":\"FetchThemAll\"}"                    
                statusIs 200

                -- Generic fetch, should be 8
                (v::Object) <- getJsonBody
                liftIO $ case parse (locateItem "queryItems") v of 
                    Error s -> expectationFailure s
                    Success (a::[Object]) -> do
                        (length a) `shouldBe` 8

            it "Query all items within 1m, should be 1" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Retrieve the items using GQL
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ fromStrict . encodeUtf8 $ "{\"query\":\"" <> (gqlQueryItemsD "1") <> 
                        "\", \"operationName\":\"FetchThemAll\"}"                    
                statusIs 200

                -- Generic fetch, should be 1
                (v::Object) <- getJsonBody
                liftIO $ case parse (locateItem "queryItems") v of 
                    Error s -> expectationFailure s
                    Success (a::[Object]) -> do
                        (length a) `shouldBe` 1

            it "Query all items within 80m, should be 3" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Retrieve the items using GQL
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ fromStrict . encodeUtf8 $ "{\"query\":\"" <> (gqlQueryItemsD "80") <> 
                        "\", \"operationName\":\"FetchThemAll\"}"                    
                statusIs 200

                -- Generic fetch, should be 1
                (v::Object) <- getJsonBody
                liftIO $ case parse (locateItem "queryItems") v of 
                    Error s -> expectationFailure s
                    Success (a::[Object]) -> do
                        (length a) `shouldBe` 3

            it "Insert an item" $ do

                -- Log in to get the token
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                token <- itoken <$> getJsonBody
                statusIs 200

                -- Retrieve the items using GQL
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    addRequestHeader (mk "Accept", "application/json")
                    addRequestHeader (mk "Authorization", encodeUtf8 $ "Bearer " <> token)
                    setUrl GQLR
                    setRequestBody $ fromStrict . encodeUtf8 $ "{\"query\":\"" <> (gqlQueryItemsD "80") <> 
                        "\", \"operationName\":\"FetchThemAll\"}"                    
                statusIs 200

    where


        -- |Create a parser for a specific field
        drill::(FromJSON a)=>Text->Object->Parser a
        drill = flip (.:)

        -- |Retrieve the response data
        locateItem::(FromJSON a) => Text -- ^The field below the data filed in the graphQL response
            -> Object                    -- ^The json structure
            -> Parser a                  -- ^The parser for the "a"-structure
        locateItem f o = do
            drill "data" o >>= drill f

        -- |Retrieves the string array of graphql types from a schema request
        locateSchema::Object    -- ^The json structure
            -> Parser [String]  -- ^The parser for the schema structure
        locateSchema o = do
            s <- drill "data" o >>= drill "__schema" >>= drill "types"
            sequence $ drill "name" <$> s