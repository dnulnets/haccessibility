-- |
-- | The Heat Application Monad module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Application(
    Environment,
    runApplication,
    default,
    ApplicationM) where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Type.Equality (class TypeEquals, from)
import Data.Array(concat)
import Data.Traversable (sequence)
import Data.Time.Duration (Milliseconds(..))

-- Effects
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as REF
import Effect.Console (log)

-- Control Monad stuff
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Parallel (parSequence, parOneOf)

-- Halogen
import Halogen as H

-- Routing imports
import Routing.Duplex (print, parse)
import Routing.Hash (setHash, getHash)

-- Our own imports
import Accessability.Interface.Endpoint (BaseURL(..))
import Accessability.Interface.Endpoint as EP
import Accessability.Interface.Authenticate (UserInfo,class ManageAuthentication)
import Accessability.Interface.Navigate (class ManageNavigation)
import Accessability.Interface.Item (class ManageItem)
import Accessability.Interface.Entity(class ManageEntity)

import Accessability.Data.Route (routeCodec, Page(..))

import Accessability.Utils.Request (
  mkRequest, 
  mkAuthRequest,
  RequestMethod (..))

-- | The application environment
type Environment = { baseURL :: BaseURL -- ^The base URL for the API
  , userInfo :: Ref (Maybe UserInfo)    -- ^The user info when logged in
  , iothubURL :: BaseURL                -- ^The url to the IoT Hub in Sundsvall
  , timeoutIothub :: Milliseconds       -- ^The timeout for iot hub API
  , timeoutBackend :: Milliseconds      -- ^The timeout for our own backend
  }

-- |The default environment
default::Ref (Maybe UserInfo) -> Environment
default ui = { baseURL : BaseURL "https://127.0.0.1"          -- ^Defuault backend, needs to changed
  , iothubURL : BaseURL "https://iotsundsvall.se/ngsi-ld/v1"  -- ^Fixed backend for the Swedish IoT Hub
  , timeoutIothub : Milliseconds 2000.0                       -- ^The timeout for the Swedish IoT Hub, it is slow
  , timeoutBackend : Milliseconds 500.0                       -- ^The timeout for our own haskell backend
  , userInfo : ui
  }

-- | The application monad
newtype ApplicationM a = ApplicationM (ReaderT Environment Aff a)

-- | Run the application monad and expose the inner Aff monad
runApplication :: Environment -- ^ The environment
  → ApplicationM              -- ^ The monad
  ~> Aff                      -- ^ An Aff result
runApplication env (ApplicationM m) = runReaderT m env

-- | Derive all monad functions
derive newtype instance functorApplication ∷ Functor ApplicationM
derive newtype instance applyApplication ∷ Apply ApplicationM
derive newtype instance applicativeApplication ∷ Applicative ApplicationM
derive newtype instance bindApplication ∷ Bind ApplicationM
derive newtype instance monadApplication ∷ Monad ApplicationM
derive newtype instance monadEffectApplication ∷ MonadEffect ApplicationM
derive newtype instance monadAffApplication ∷ MonadAff ApplicationM

-- | ask implementation
instance monadAskApplication ∷ TypeEquals e Environment ⇒ MonadAsk e ApplicationM where
  ask = ApplicationM $ asks from

-- | Reload the current page
--reload :: Effect Unit
--reload = window >>= location >>= L.reload

--
-- Add the set of functions that handles navigation in the app
--
instance manageNavigationApplicationM ∷ ManageNavigation ApplicationM where

  -- |Navigates the app using hash based routing
  gotoPage newPage = do
    H.liftEffect $ log $ "GotoPage to = " <> (show newPage)
    oldHash <- H.liftEffect $ getHash
    H.liftEffect $ log $ "Current page = " <> (show $ page oldHash)
    if newPage /= page oldHash
      then do
        H.liftEffect $ log $ "Set hash to " <> newHash
        H.liftEffect $ setHash $ newHash
      else do
        H.liftEffect $ log $ "Reload hash with " <> oldHash
        -- H.liftEffect $ reload
    where
      newHash :: String
      newHash = print routeCodec newPage

      page :: String->Page
      page h = either (const Error) identity $ parse routeCodec h

--      reload :: Effect Unit
--      reload = window >>= location >>= L.reload

--
--  Add the set of functions that handles login and logout of a user
--
instance manageAuthenticationApplicationM :: ManageAuthentication ApplicationM where

  -- |Tries to login the user and get a token from the backend that can be used for future
  -- calls
  login auth = do
    ref <- asks _.userInfo
    burl <- EP.backend ep
    response <- liftAff $ mkRequest burl EP.Authenticate (Post (Just auth))    
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        H.liftEffect $ REF.write Nothing ref
        pure Nothing
      Right (Tuple _ userInfo) -> do
        H.liftEffect $ REF.write userInfo ref
        pure userInfo
    where
      ep = EP.Authenticate

  -- |Log out the user
  logout = do
    ref <- asks _.userInfo
    H.liftEffect $ REF.write Nothing ref

--
--  Add the set of functions that handles items
--
instance manageItemApplicationM :: ManageItem ApplicationM where

  -- |Tries to login the user and get a token from the backend that can be used for future
  -- calls
  queryItems filter = do
    ref <- asks _.userInfo    
    ui <- H.liftEffect $ REF.read ref
    burl <- EP.backend ep
    response <- liftAff $ mkAuthRequest burl ep ui (Post (Just filter))    
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure Nothing
      Right (Tuple _ items) -> do
        pure items
    where
      ep = EP.Items

--
--  Add the set of functions that handles entities from the IoT Hub
--
instance manageEntityApplicationM :: ManageEntity ApplicationM where

  -- |Executes the queries towards the backend in parallel to get all entities
  -- andmerge them into one response
  queryEntities et = do
    b1 <- EP.backend ep1
    b2 <- EP.backend ep2
    tmo <- asks _.timeoutIothub
    response <- liftAff $ (map concat) <$> (parOneOf [
      sequence <$> parSequence [
        unpack <$> mkRequest b1 ep1 (Get::RequestMethod Void)
        , unpack <$> mkRequest b2 ep2 (Get::RequestMethod Void)
      ],
      Nothing <$ (delay tmo)])

    pure response

    where
      ep1 = EP.Entities {type: et, attrs: Just "temperature"}
      ep2 = EP.Entities {type: et, attrs: Just "snowHeight"}
      unpack (Left _) = Nothing
      unpack (Right (Tuple _ e)) = Just e
