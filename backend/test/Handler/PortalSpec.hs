{-# LANGUAGE OverloadedStrings #-}
module Handler.PortalSpec (spec) where

import TestPlatform

spec :: Spec
spec = withApp $ do
        describe "Test the presence of the portal application" $ do
            it "Loads the portals home page" $ do
                get $ StaticR index_html
                statusIs 200
