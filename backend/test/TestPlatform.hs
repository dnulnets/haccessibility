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
import qualified Data.ByteString.Lazy as DBL
import           Data.Text                            as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified Test.HUnit as HUnit

import           Database.Persist                     hiding (get)
import           Database.Persist.Postgresql
import           Database.Persist.Sql                 (SqlPersistM,
                                                       connEscapeName,
                                                       rawExecute, rawSql,
                                                       runSqlPersistMPool,
                                                       unSingle)
import          Database.Persist.Sql as X (SqlPersistM)
import           Network.Wai.Middleware.RequestLogger
import Network.Wai.Test
import           WaiAppStatic.Storage.Filesystem      (defaultWebAppSettings)
import           WaiAppStatic.Types                   (StaticSettings (..))

import           System.Environment                   (getEnv)
import           System.Random
import           Test.Hspec as X

import           Yesod
import           Yesod.Static
import           Yesod.Test as X

import Data.Time.Clock
import Data.Time.Clock.System

import Data.Aeson

import Network.HTTP.Types.Header as X
import Data.CaseInsensitive as X

mkYesodDispatch "Server" resourcesServer

staticFiles "static"

sec::YesodExample Server NominalDiffTime
sec = liftIO $ fromIntegral . systemSeconds <$> getSystemTime

runDB :: SqlPersistM a -> YesodExample Server a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: Server -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (serverConnectionPool app)

cleanDB::YesodExample Server ()
cleanDB = do
    app <- getTestYesod
    liftIO $ runDBWithApp app $ do
        tables <- getTables
        sqlBackend <- ask

        let escapedTables = fmap (connEscapeName sqlBackend . DBName) tables
            query = "TRUNCATE TABLE " <> (intercalate ", " escapedTables)
        rawExecute query []

makeServer ::IO Server
makeServer = do
    gen <- newStdGen
    let jwtSecret = Prelude.take 10 $ randomRs ('a','z') gen
    database <- getEnv "HAPI_DATABASE"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"
    time <- read <$> getEnv "HAPI_JWT_SESSION_LENGTH"
    pool <- runNoLoggingT $ createPostgresqlPool (DB.pack database) 5
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

withCleanApp :: SpecWith (TestApp Server) -> Spec
withCleanApp = before $ do
    foundation <- makeServer
    wipeDB foundation
    return $ testApp foundation logStdoutDev

withBaseDataApp :: SqlPersistM a->SpecWith (TestApp Server) -> Spec
withBaseDataApp setup = before $ do
    foundation <- makeServer
    wipeDB foundation
    runDBWithApp foundation setup
    return $ testApp foundation logStdoutDev

withBaseDataAppOnce :: SqlPersistM a->SpecWith (TestApp Server) -> Spec
withBaseDataAppOnce setup = beforeAll $ do
    foundation <- makeServer
    wipeDB foundation
    runDBWithApp foundation setup
    return $ testApp foundation logStdoutDev

wipeDB :: Server -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = fmap (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " <> (intercalate ", " escapedTables)
    rawExecute query []

getTables :: SqlPersistM [Text]
getTables = do
    tables <- rawSql "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE';" []

    return $ unSingle <$> tables


-- Yes, just a shortcut
failure :: (MonadIO a) => T.Text -> a b
failure reason = (liftIO $ HUnit.assertFailure $ T.unpack reason) >> error ""

getJsonBody::(FromJSON a)=>YesodExample site a
getJsonBody = withResponse jsonBody

jsonBody::(FromJSON a)=>SResponse -> YesodExample site a
jsonBody (SResponse status header body) = do
    case eitherDecode' body of
        Left err -> do
          let characterLimit = 1024
              textBody = TL.toStrict $ DTLE.decodeUtf8 body
              bodyPreview = if T.length textBody < characterLimit
                then textBody
                else T.take characterLimit textBody <> "... (use `printBody` to see complete response body)"
          failure $ T.concat ["Failed to parse JSON response; error: ", T.pack err, "JSON: ", bodyPreview]            
        Right v -> return v
