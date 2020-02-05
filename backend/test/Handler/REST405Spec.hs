{-# LANGUAGE OverloadedStrings #-}
module Handler.REST405Spec (spec) where

import TestPlatform

spec :: Spec
spec = withApp $ do
        describe "Test Methods that are not allowed or do not exist - HTTP Response 403, 405" $ do
            it "GET on /" $ do
                request $ do
                    setMethod "GET"
                statusIs 404
            it "GET on GQL" $ do
                get GQLR
                statusIs 405
            it "GET /api/item" $ do
                get CreateItemR
                statusIs 405
            it "DELETE /api/item" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl CreateItemR
                statusIs 405
            it "PUT /api/item" $ do
                request $ do
                    setMethod "PUT"
                    setUrl CreateItemR
                statusIs 405
            it "POST /api/item/0000000000000001" $ do
                request $ do
                    setMethod "POST"
                    setUrl $ ItemR "0000000000000001"
                statusIs 405
            it "GET /api/items" $ do
                get ItemsR
                statusIs 405
            it "DELETE /api/items" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl ItemsR
                statusIs 405
            it "PUT /api/items" $ do
                request $ do
                    setMethod "PUT"
                    setUrl ItemsR
                statusIs 405
            it "GET /api/authenticate" $ do
                get AuthenticateR
            it "PUT /api/authenticate" $ do
                request $ do
                    setMethod "PUT"
                    setUrl AuthenticateR
                statusIs 405
            it "DELETE /api/authenticate" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl AuthenticateR
                statusIs 405



