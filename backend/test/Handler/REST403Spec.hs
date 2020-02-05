{-# LANGUAGE OverloadedStrings #-}
module Handler.REST403Spec (spec) where

import TestPlatform

spec :: Spec
spec = withApp $ do
        describe "Test Methods that needs authentication, but are not - HTTP Response 403" $ do
            it "POST /gql" $ do
                request $ do
                    setMethod "POST"
                    setUrl GQLR
                statusIs 403
            it "POST /api/item" $ do
                request $ do
                    setMethod "POST"
                    setUrl CreateItemR
                statusIs 403
            it "GET /api/item/0000000000000001" $ do
                get $ ItemR "0000000000000001"
                statusIs 403
            it "PUT /api/item/0000000000000001" $ do
                request $ do
                    setMethod "PUT"
                    setUrl $ ItemR "0000000000000001"
                statusIs 403
            it "DELETE /api/item/0000000000000001" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl $ ItemR "0000000000000001"
                statusIs 403
            it "POST /api/items" $ do
                request $ do
                    setMethod "POST"
                    setUrl ItemsR
                statusIs 403
