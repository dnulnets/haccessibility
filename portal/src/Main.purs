-- |
-- | The Main module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Main where

-- Get the build constant
import Version (build)

-- Standard imports
import Prelude
import Data.Nullable (toMaybe)
import Data.Maybe (Maybe(..))

-- Effect imports
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Effect.Console (log)

-- Halogen imports
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- Web imports
import Web.HTML (window)                                                         
import Web.HTML.Navigator.Geolocation (geolocation) 
import Web.HTML.Window (navigator, location)
import Web.HTML.Location (origin)

-- Our own imports
import Accessability.Application (runApplication, Environment)
import Accessability.Root as Root
import Accessability.Interface.Endpoint (BaseURL(..))

-- | Hoist in our Application monad
rootComponent ∷ ∀ i q. Environment →                -- ^ The Environment
                H.Component HH.HTML q i Void Aff    -- ^ The Application root component
rootComponent env = H.hoist (runApplication env) Root.component

-- | The main entry point for our application
main ∷ Effect Unit -- ^ Default return value
main = do
  currentUserInfo <- liftEffect $ Ref.new Nothing
  navloc <- window >>= navigator >>= geolocation
  loc <- window >>= location >>= origin
  log $ "Origin = " <> loc
  log $ "Build = " <> build
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      env ∷ Environment
      env = { geo : toMaybe navloc
        , baseURL : BaseURL loc
        , userInfo : currentUserInfo}
    runUI (rootComponent env) unit body