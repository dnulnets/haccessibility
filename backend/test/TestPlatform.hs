{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module TestPlatform (module TestPlatform, module X) where

import           Control.Monad.Logger
import           Control.Monad.Reader

import           Accessability.Foundation as X
import           Accessability.Settings
import           Accessability.Handler.GQL          (postGQLR)

import           Accessability.Handler.REST.Item         (deleteItemR, getItemR,
                                                     postCreateItemR,
                                                     postItemsR, putItemR)

import           Accessability.Handler.REST.Authenticate (postAuthenticateR)

import qualified Data.ByteString.Char8                as DB
import           Data.Text                            hiding (take)

import           Database.Persist                     hiding (get)
import           Database.Persist.Postgresql
import           Database.Persist.Sql                 (SqlPersistM,
                                                       connEscapeName,
                                                       rawExecute, rawSql,
                                                       runSqlPersistMPool,
                                                       unSingle)
import           Network.Wai.Middleware.RequestLogger
import           WaiAppStatic.Storage.Filesystem      (defaultWebAppSettings)
import           WaiAppStatic.Types                   (StaticSettings (..))

import           System.Environment                   (getEnv)
import           System.Random
import           Test.Hspec as X

import           Yesod
import           Yesod.Static
import           Yesod.Test as X

mkYesodDispatch "Server" resourcesServer

staticFiles "static"

runDB :: SqlPersistM a -> YesodExample Server a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: Server -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (serverConnectionPool app)

makeServer ::IO Server
makeServer = do
    gen <- newStdGen
    let jwtSecret = take 10 $ randomRs ('a','z') gen
    database <- getEnv "HAPI_DATABASE"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"
    time <- read <$> getEnv "HAPI_JWT_SESSION_LENGTH"
    pool <- runStderrLoggingT $ createPostgresqlPool (DB.pack database) 5
    return Server {
        getStatic = Static $ (defaultWebAppSettings "static") {ssUseHash = False},
        appSettings = defaultSettings {
            tokenSecret = pack jwtSecret,
            passwordCost = cost,
            tokenExpiration = time},
        serverConnectionPool = pool }

withApp :: SpecWith (TestApp Server) -> Spec
withApp = before $ do
    foundation <- makeServer
    return $ testApp foundation logStdoutDev

wipeDB :: Server -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    liftIO $ putStrLn $ show tables
    sqlBackend <- ask

    let escapedTables = fmap (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " <> (intercalate ", " escapedTables)
    rawExecute query []

getTables :: SqlPersistM [Text]
getTables = do
    tables <- rawSql "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE';" []

    return $ unSingle <$> tables

