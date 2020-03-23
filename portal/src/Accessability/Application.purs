-- |
-- | The Heat Application Monad module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Application(
    Environment,
    runApplication,
    ApplicationM) where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Type.Equality (class TypeEquals, from)

-- Effects
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as REF
import Effect.Console (log)

-- Control Monad stuff
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)

-- Geolocation
import Web.HTML.Navigator.Geolocation (NavigatorGeolocation)

-- Halogen
import Halogen as H

-- Routing imports
import Routing.Duplex (print)
import Routing.Hash (setHash)

-- Our own imports
import Accessability.Interface.Endpoint (BaseURL)
import Accessability.Interface.Endpoint as EP
import Accessability.Interface.Authenticate (UserInfo,
  class ManageAuthentication)
import Accessability.Utils.Request (mkRequest,
                           RequestMethod (..))
import Accessability.Interface.Navigate (class ManageNavigation)
import Accessability.Data.Route (routeCodec)

-- | The application environment
type Environment = {
  geo ∷ Maybe NavigatorGeolocation    -- ^ The geolocation handle
  , baseURL :: BaseURL                -- ^ The base URL for the API
  , userInfo :: Ref (Maybe UserInfo)  -- ^ The user info when logged in
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
  --gotoPage newPage = do
  --  H.liftEffect $ setHash $ print routeCodec newPage
  gotoPage newPage = do
    H.liftEffect $ setHash $ print routeCodec newPage

--
--  Add the set of functions that handles login and logout of a user
--
instance manageAuthenticationApplicationM :: ManageAuthentication ApplicationM where

  -- |Tries to login the user and get a token from the backend that can be used for future
  -- calls
  login auth = do
    ref <- asks _.userInfo
    response <- mkRequest EP.Authenticate (Post (Just auth))
    
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        H.liftEffect $ REF.write Nothing ref
        pure Nothing
      Right (Tuple _ userInfo) -> do
        H.liftEffect $ REF.write userInfo ref
        pure userInfo

  -- |Log out the user
  logout = do
    ref <- asks _.userInfo
    H.liftEffect $ REF.write Nothing ref
