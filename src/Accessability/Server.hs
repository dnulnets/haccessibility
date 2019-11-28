{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Accessability.Server (Server(..)) where

import Yesod
import           Database.Persist
import           Database.Persist.Postgresql
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types ( status200
    , status201
    , status400
    , status403
    , status404
    )
import Data.Text (Text)
import Data.Morpheus.Types (GQLRequest(..), GQLResponse(..))
import Accessability.API(api)

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.
data Server = Server ConnectionPool

-- We'll create a single route, to access a person. It's a very common
-- occurrence to use an Id type in routes.
mkYesod "Server" [parseRoutes|
/ GQLR POST
|]

-- Nothing special here
instance Yesod Server

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist Server where
    type YesodPersistBackend Server = SqlBackend

    runDB action = do
        Server pool <- getYesod
        runSqlPool action pool

-- List all people in the database
postGQLR :: Handler Value
postGQLR = do
    request <- requireJsonBody::Handler GQLRequest
    response <- liftIO $ api request
    sendStatusJSON status201 response

openConnectionCount :: Int
openConnectionCount = 10

--main :: IO ()
--main = runStderrLoggingT $ withSqlitePool "test.db3" openConnectionCount $ \pool -> liftIO $ do
--    runResourceT $ flip runSqlPool pool $ do
--        runMigration migrateAll
--        insert $ Person "Michael" "Snoyman" 26
--    warp 3000 $ PersistTest pool
