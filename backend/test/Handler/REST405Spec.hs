{-# LANGUAGE OverloadedStrings #-}
module Handler.REST405Spec (spec) where

import TestPlatform

spec :: Spec
spec = withApp $ do
        describe "Try using methods that is not supporte don the api" $ do
            it "Try to get the root" $ do
                request $ do
                    setMethod "GET"
                statusIs 404
            it "Try to get the GraphQL interface" $ do
                get GQLR
                statusIs 405
            it "Try to get the create item interface" $ do
                get CreateItemR
                statusIs 405
            it "Try to delete an item without a key" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl CreateItemR
                statusIs 405
            it "Try to update an item without a key" $ do
                request $ do
                    setMethod "PUT"
                    setUrl CreateItemR
                statusIs 405
            it "Try to create an item given a key" $ do
                post $ ItemR "0000000000000001"
                statusIs 405
            it "Try to get the search interface" $ do
                get ItemsR
                statusIs 405
            it "Try to delete the search interface" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl ItemsR
                statusIs 405
            it "Try to update the search interface" $ do
                request $ do
                    setMethod "PUT"
                    setUrl ItemsR
                statusIs 405
            it "Try to get the authenticate interface" $ do
                get AuthenticateR
            it "Try to put the authenticate interface" $ do
                request $ do
                    setMethod "PUT"
                    setUrl AuthenticateR
                statusIs 405
            it "Try to delete the authenticate interface" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl AuthenticateR
                statusIs 405