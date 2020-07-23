-- |
-- | The Heat Application Monad module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Application (
  Environment
  , runApplication
  , default
  , ApplicationM) where

-- Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Tuple (Tuple(..))
import Data.Array(concat)
import Data.Traversable (sequence, sequence_)
import Data.Time.Duration (Milliseconds(..))
import Data.Newtype (unwrap)

-- Type imports
import Type.Equality (class TypeEquals, from)

-- Effects
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as REF
import Effect.Console (log)

-- AffJax
import Affjax.StatusCode as AX

-- Monad stuff
import Control.Monad.Reader (asks, ask, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Parallel (parSequence, parOneOf)

-- Halogen
import Halogen as H

-- Routing imports
import Routing.Duplex (print, parse)
import Routing.Hash (setHash, getHash)

-- Our own imports
import Accessibility.Interface.Endpoint (BaseURL(..), Problem(..))
import Accessibility.Interface.Endpoint as EP
import Accessibility.Interface.Authenticate (UserInfo(..),class ManageAuthentication)
import Accessibility.Interface.Navigate (class ManageNavigation)
import Accessibility.Interface.Item (class ManageItem)
import Accessibility.Interface.Entity(class ManageEntity)

import Accessibility.Data.Route (routeCodec, Page(..))

import Accessibility.Utils.Token (removeToken, writeToken, Token(..))
import Accessibility.Utils.Request (
--  mkRequest, 
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

--
-- Add the set of functions that handles navigation in the app
--
instance manageNavigationApplicationM ∷ ManageNavigation ApplicationM where

  -- |Navigates the app using hash based routing
  gotoPage newPage = do
    H.liftEffect $ log $ "GotoPage: To = " <> (show newPage)
    oldHash <- H.liftEffect $ getHash
    H.liftEffect $ log $ "Current: Page = " <> (show $ page oldHash)
    
    if newPage /= page oldHash
      then do
        H.liftEffect $ log $ "Set hash to: " <> newHash
        H.liftEffect $ setHash $ newHash
      else do
        H.liftEffect $ log $ "Reload hash with: " <> oldHash

    where
      newHash :: String
      newHash = print routeCodec newPage

      page :: String->Page
      page h = either (const Error) identity $ parse routeCodec h


--
--  Add the set of functions that handles login and logout of a user
--
instance manageAuthenticationApplicationM :: ManageAuthentication ApplicationM where

  -- |Tries to login the user and get a token from the backend that can be used for future
  -- calls
  login auth = do
    env <- ask
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
      mkAuthRequest burl ep Nothing (Post (Just auth))
      , Left "Timeout" <$ (delay env.timeoutBackend)]

    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        H.liftEffect $ REF.write Nothing env.userInfo
        H.liftEffect $ removeToken
        pure Nothing
      Right (Tuple _ userInfo) -> do
        H.liftEffect $ REF.write userInfo env.userInfo
        H.liftEffect $ sequence_ $ writeToken <$> (Token <$> getToken <$> userInfo)
        pure userInfo
    where
      ep = EP.Authenticate
      getToken (UserInfo r) = r.token

  -- |Log out the user
  logout = do
    ref <- asks _.userInfo
    H.liftEffect $ REF.write Nothing ref
    H.liftEffect $ removeToken

--
--  Add the set of functions that handles items
--
instance manageItemApplicationM :: ManageItem ApplicationM where

  -- |Add an item
  updateItem item = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
        mkAuthRequest burl ep (_.token <<< unwrap <$> ui) (Put (Just item))
        , Left "Timeout" <$ (delay env.timeoutBackend)]
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure Nothing
      Right (Tuple _ new) -> do
        pure new

    where

      ep = EP.Item item.id

  -- |Add an item
  addItem item = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
        mkAuthRequest burl ep (_.token <<< unwrap <$> ui) (Post (Just item))
        , Left "Timeout" <$ (delay env.timeoutBackend)]
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure Nothing
      Right (Tuple _ new) -> do
        pure new
    where
      ep = EP.Item Nothing

  -- |Gets all available attributes
  queryAttributes = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
      mkAuthRequest burl ep (_.token <<< unwrap <$> ui) (Get::RequestMethod Void)
      , Left "Timeout" <$ (delay env.timeoutBackend)]
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure $ Left Backend
      Right (Tuple (AX.StatusCode 403) attrs) -> do
        pure $ Left NotAuthenticated
      Right (Tuple _ attrs) -> do
        pure $ Right attrs
    where
      ep = EP.Attributes      

  -- |Update all attributes in the item
  updateItemAttributes key arr = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
      mkAuthRequest burl ep (_.token <<< unwrap <$> ui) $ Put (Just arr)
      , Left "Timeout" <$ (delay env.timeoutBackend)]
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure Nothing
      Right (Tuple _ v) -> do      
        pure $ Just v
    where
      ep = EP.Attribute key

  -- |Gets all available attributes
  queryItemAttributes key = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
      mkAuthRequest burl ep (_.token <<< unwrap <$> ui) (Get::RequestMethod Void)
      , Left "Timeout" <$ (delay env.timeoutBackend)]
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure $ Left Backend
      Right (Tuple (AX.StatusCode 403) _) -> do
        pure $ Left NotAuthenticated
      Right (Tuple _ attrs) -> do
        pure $ Right attrs
    where
      ep = EP.Attribute key

  -- |Get the item based on its key
  queryItem key = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
      mkAuthRequest burl ep (_.token <<< unwrap <$> ui) (Get::RequestMethod Void)
      , Left "Timeout" <$ (delay env.timeoutBackend)]

    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure $ Left Backend
      Right (Tuple (AX.StatusCode 403) _) -> do
        pure $ Left NotAuthenticated
      Right (Tuple _ item) -> do
        pure $ Right item

    where
      ep = EP.Item (Just key)

  -- |Get all items based on a filter
  queryItems filter = do
    env <- ask
    ui <- H.liftEffect $ REF.read env.userInfo
    burl <- EP.backend ep

    response <- liftAff $ parOneOf [
      mkAuthRequest burl ep (_.token <<< unwrap <$> ui) (Post (Just filter))
      , Left "Timeout " <$ (delay env.timeoutBackend)]

    case response of
      Left err -> do
        pure (Left Backend)
      Right (Tuple (AX.StatusCode 403) _) -> do
        pure (Left NotAuthenticated)
      Right (Tuple sts items) -> do
        pure (Right items)

    where
      ep = EP.Items

--
--  Add the set of functions that handles entities from the IoT Hub
--
instance manageEntityApplicationM :: ManageEntity ApplicationM where

  -- |Executes the queries towards the backend in parallel to get all entities
  -- and merge them into one response
  queryEntities et = do
    b1 <- EP.backend ep1
    b2 <- EP.backend ep2
    env <- ask

    response <- liftAff $ parOneOf [
      (map concat) <$> (sequence <$> parSequence [
        unpack <$> mkAuthRequest b1 ep1 Nothing (Get::RequestMethod Void)
--        , unpack <$> mkRequest b2 ep2 (Get::RequestMethod Void)
      ]),
      (Left Backend) <$ (delay env.timeoutIothub)]

    pure response

    where
      ep1 = EP.Entities {type: et, attrs: Just "temperature"}
      ep2 = EP.Entities {type: et, attrs: Just "snowHeight"}
      unpack (Left _) = Left Backend
      unpack (Right (Tuple (AX.StatusCode 403) e)) = Left NotAuthenticated
      unpack (Right (Tuple _ e)) = Right e
