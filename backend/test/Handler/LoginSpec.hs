{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.LoginSpec (spec) where

import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Aeson (Value(..))
import Database.Persist hiding (get)

import TestPlatform

import Yesod (liftIO)

import Accessability.Model.Database
import Accessability.Model.REST.Authenticate
import Accessability.Model.Transform
import Accessability.Utils.JWT
import Accessability.Settings

-- |Sets up the user that we run all the tests against.
baseData::SqlPersistM ()
baseData = do
    _ <- insert $ User "test" "$2b$10$Mrwvk0uFy/ZekcDeyGTyd.Lyx3k380XuB5zq1yaVxqEd6y5SxSrcG" "test@testland.com"     
    return ()

-- |The test specification
spec :: Spec
spec = withBaseDataAppOnce baseData $ do

        describe "Login failures and successes with correct body" $ do

            it "Login with wrong credentials" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"fewhtrhtfcw\"}"
                statusIs 401

            it "Login with correct credentials" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                (ui::UserInfo)<-getJsonBody
                statusIs 200

            it "Login with correct credentials, and test the token validity" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                settings <- appSettings <$> getTestYesod
                seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
                udb <- runDB $ getBy $ UniqueUserUsername "test"
                ut <- getJsonBody
                liftIO $ case tokenToJson (tokenSecret settings) seconds (itoken ut) of
                    Just v -> case v of
                        String t -> shouldBe t $ fromMaybe "" (keyToText <$> (entityKey <$> udb))
                        _ -> expectationFailure "Did not get the string key from the JSON value of the token"
                    Nothing -> expectationFailure "Did not get a JSON value from the token"

            it "Login with correct credentials, and test the token has timed out" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                settings <- appSettings <$> getTestYesod
                --seconds <- liftIO $ fromIntegral <$> ((+ 24*60*60::Int64) . systemSeconds <$> getSystemTime)
                seconds <- liftIO $ fromIntegral <$> ((+ (fromInteger (tokenExpiration settings + 1))) . systemSeconds <$> getSystemTime)
                udb <- runDB $ getBy $ UniqueUserUsername "test"
                ut <- getJsonBody
                liftIO $ shouldBe Nothing $ tokenToJson (tokenSecret settings) seconds (itoken ut)

        describe "Login failures due to malformed body" $ do

            it "Login with faulty fieldnames" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"usernae\":\"dwqfewdew\",\"password\":\"fewhtrhtfcw\"}"
                statusIs 400

            it "Login with missing fieldnames" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"password\":\"fewhtrhtfcw\"}"
                statusIs 400