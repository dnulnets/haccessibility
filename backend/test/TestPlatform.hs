{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module TestPlatform (module TestPlatform, module X) where

import           Control.Monad.Logger
import           Control.Monad.Reader

import           Accessability.Foundation                as X
import           Accessability.Handler.GQL               (postGQLR)
import           Accessability.Settings

import           Accessability.Handler.REST.Item         (deleteItemR, getItemR,
                                                          postCreateItemR,
                                                          postItemsR, putItemR)

import           Accessability.Handler.REST.Authenticate (postAuthenticateR)
import           Accessability.Model.Database            (entityDefs)

import qualified Data.ByteString.Char8                   as DB
import qualified Data.ByteString.Lazy                    as DBL
import           Data.Text                               as T
import qualified Data.Text.Lazy                          as TL
import qualified Data.Text.Lazy.Encoding                 as DTLE
import qualified Test.HUnit                              as HUnit

import           Database.Persist                        hiding (get)
import           Database.Persist.Postgresql
import           Database.Persist.Sql                    (SqlPersistM,
                                                          connEscapeName,
                                                          rawExecute, rawSql,
                                                          runSqlPersistMPool,
                                                          unSingle)
import           Database.Persist.Sql                    as X (SqlPersistM)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Test
import           WaiAppStatic.Storage.Filesystem         (defaultWebAppSettings)
import           WaiAppStatic.Types                      (StaticSettings (..))

import           System.Environment                      (getEnv)
import           System.Random
import           Test.Hspec                              as X

import           Yesod
import           Yesod.Static
import           Yesod.Test                              as X

import           Data.Time.Clock
import           Data.Time.Clock.System

import           Data.Aeson

import           Data.CaseInsensitive                    as X
import           Network.HTTP.Types.Header               as X

--
-- Yesod setup
--
mkYesodDispatch "Server" resourcesServer
mkMigrate "migrateAll" entityDefs
staticFiles "static"

-- |Run a database action within the yesod monad
runDB :: SqlPersistM a
    -> YesodExample Server a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

-- |Runs a database action within the IO monad
runDBWithApp :: Server          -- ^ The yesod foundation
    -> SqlPersistM a -> IO a    -- ^ The database action
runDBWithApp app query = runSqlPersistMPool query (serverConnectionPool app)

-- |Makes the server for the tests.
makeServer ::IO Server
makeServer = do
    gen <- newStdGen
    let jwtSecret = Prelude.take 10 $ randomRs ('a','z') gen
    database <- getEnv "HAPI_DATABASE"
    cost <- read <$> getEnv "HAPI_PASSWORD_COST"
    time <- read <$> getEnv "HAPI_JWT_SESSION_LENGTH"
    -- pool <- runNoLoggingT $ createPostgresqlPool (DB.pack database) 5
    pool <- runStderrLoggingT $ createPostgresqlPool (DB.pack database) 5
    return Server {
        getStatic = Static $ (defaultWebAppSettings "static") {ssUseHash = False},
        appSettings = defaultSettings {
            tokenSecret = pack jwtSecret,
            passwordCost = cost,
            tokenExpiration = time},
        serverConnectionPool = pool }

-- |Runs before every "it" in the Spec, no database wiping
withApp :: SpecWith (TestApp Server) -> Spec
withApp = before $ do
    foundation <- makeServer
    migrateDB foundation
    return $ testApp foundation logStdoutDev

-- |Wipes the database and make it run before every "it" in the Spec
withCleanApp :: SpecWith (TestApp Server) -> Spec
withCleanApp = before $ do
    foundation <- makeServer
    migrateDB foundation
    wipeDB foundation
    return $ testApp foundation logStdoutDev

-- |Wipes the database and make it run a database action before every "it" in the Spec
withBaseDataApp :: SqlPersistM a->SpecWith (TestApp Server) -> Spec
withBaseDataApp setup = before $ do
    foundation <- makeServer
    migrateDB foundation
    wipeDB foundation
    runDBWithApp foundation setup
    return $ testApp foundation logStdoutDev

-- |Wipes the database and make it run a database action once for each Spec
withBaseDataAppOnce :: SqlPersistM a->SpecWith (TestApp Server) -> Spec
withBaseDataAppOnce setup = beforeAll $ do
    foundation <- makeServer
    migrateDB foundation
    wipeDB foundation
    runDBWithApp foundation setup
    return $ testApp foundation logStdoutDev

-- |Migrate the database, if any changes has been made by persist
migrateDB:: Server -> IO ()
migrateDB app = runDBWithApp app $ do
    runMigration migrateAll

-- |Truncates all tables in the database that have been defined by persist
wipeDB :: Server -> IO ()
wipeDB app = runDBWithApp app $ do
    sqlBackend <- ask
    let tables = (unDBName . entityDB) <$> entityDefs
        escapedTables = fmap (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " <> (intercalate ", " escapedTables)
    rawExecute query []

-- |Transforms the last response body to a JSON object
getJsonBody::(FromJSON a)=>YesodExample site a
getJsonBody = withResponse jsonBody
    where
        failure :: (MonadIO a) => T.Text -> a b
        failure reason = (liftIO $ HUnit.assertFailure $ T.unpack reason) >> error ""

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

-- |Determine if the NominalDiffTime is less than 1 second.
timeProximity::NominalDiffTime -- ^ The difference in seconds
    ->Bool                     -- ^ If it is close enough in time
timeProximity n = (abs n) < 1.0

-- |Determine if the longitude or latitude difference is less than 0.000001, it is considered enough
-- to be able to distinguish individuals on the map, i.e. around 1 dm distance
lolaProximity::Float    -- ^ The difference in degrees
    ->Bool              -- ^ If it is close enough
lolaProximity d = (abs d) < 0.000001
