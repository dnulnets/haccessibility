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

spec :: Spec
spec = do
    withBaseDataApp baseData $ do

        describe "Login successes" $ do
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

            it "Login with correct credentials, and test the token invalidity" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"test\",\"password\":\"test\"}"
                (ui::UserInfo)<-getJsonBody
                statusIs 200

    withApp $ do

        describe "Login failures" $ do
            it "Login with wrong credentials" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"dwqfewdew\",\"password\":\"fewhtrhtfcw\"}"

                statusIs 401

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

baseData::SqlPersistM ()
baseData = do
    _ <- insert $ User "test" "$2b$10$Mrwvk0uFy/ZekcDeyGTyd.Lyx3k380XuB5zq1yaVxqEd6y5SxSrcG" "test@testland.com"     
    return ()

ssec::YesodExample Server NominalDiffTime
ssec = liftIO $ fromIntegral . systemSeconds <$> getSystemTime
