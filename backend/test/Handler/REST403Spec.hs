{-# LANGUAGE OverloadedStrings #-}
module Handler.REST403Spec (spec) where

import TestPlatform

spec :: Spec
spec = withApp $ do
        describe "Test Methods that needs authentication, but no credentials are given" $ do
            it "Try to use the graphQL interface" $ do
                post GQLR
                statusIs 403
            it "Try to create an item" $ do
                post CreateItemR
                statusIs 403
            it "Try to get an item given a key" $ do
                get $ ItemR "0000000000000001"
                statusIs 403
            it "Try to update an item given a key" $ do
                request $ do
                    setMethod "PUT"
                    setUrl $ ItemR "0000000000000001"
                statusIs 403
            it "Try to delete an item given a key" $ do
                request $ do
                    setMethod "DELETE"
                    setUrl $ ItemR "0000000000000001"
                statusIs 403
            it "Try to search or items" $ do
                post ItemsR
                statusIs 403
