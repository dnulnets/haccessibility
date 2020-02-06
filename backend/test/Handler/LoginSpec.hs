{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.LoginSpec (spec) where

import Accessability.Model.Database
import Accessability.Model.REST.Authenticate
import Database.Persist hiding (get)

import TestPlatform

baseData::SqlPersistM ()
baseData = do
    _ <- insert $ User "test" "$2b$10$Mrwvk0uFy/ZekcDeyGTyd.Lyx3k380XuB5zq1yaVxqEd6y5SxSrcG" "test@testland.com"     
    return ()

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

