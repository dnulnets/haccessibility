{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginSpec (spec) where

import TestPlatform

spec :: Spec
spec = withCleanApp $ do
        describe "Login failures" $ do
            it "POST on /api/authenticate with wrong credentials" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"username\":\"dwqfewdew\",\"password\":\"fewhtrhtfcw\"}"

                statusIs 401

            it "POST on /api/authenticate with faulty fieldnames" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"usernae\":\"dwqfewdew\",\"password\":\"fewhtrhtfcw\"}"

                statusIs 400

            it "POST on /api/authenticate with missing fieldnames" $ do
                request $ do
                    setMethod "POST"
                    addRequestHeader (mk "Content-Type","application/json")
                    setUrl AuthenticateR
                    setRequestBody "{ \"password\":\"fewhtrhtfcw\"}"

                statusIs 400

