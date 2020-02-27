-- |
-- | The nearby component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessability.Component.Nearby where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Monad.Reader (asks)

-- Effects
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Events as HE

-- Web imports
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML.Navigator.Geolocation (NavigatorGeolocation,
  getCurrentPosition,
  defaultOptions,
  Position)

-- Our own stuff
import Accessability.Component.HTML.Utils (css, style)
import Accessability.Interface.Navigate (class ManageNavigation)

-- | Slot type for the Login component
type Slot p = ∀ q . H.Slot q Void p

-- | State for the component
type State = {  alert::Maybe String,   -- ^ The alert text
                position::Maybe Position}  -- ^ The GPS position of the user

-- | Initial state is no logged in user
initialState ∷ ∀ i. i   -- ^ Initial input
  → State               -- ^ The state
initialState _ = { alert : Nothing,
                   position : Nothing }

-- | Internal form actions
data Action = GPS

-- | The component definition
component ∷ ∀ r q i o m . MonadAff m
            ⇒ ManageNavigation m
            ⇒ MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
            ⇒ H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

nearbyAlert::forall p i . Maybe String -> HH.HTML p i
nearbyAlert t = HH.b [css "", style $ ("color:red;visibility:" <> (maybe "hidden" (\_->"visible") t))] 
  [HH.text $ fromMaybe "" t]

--loginAlert t = HH.div [css "alert alert-danger alert-signin", HPA.role "alert", style "visibility: hidden"] [HH.text t]

-- | Render the alert
render ∷ ∀ m . MonadAff m ⇒ State -- ^ The state to render
  → H.ComponentHTML Action () m   -- ^ The components HTML
render state = HH.div
               []
               [HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton, HE.onClick (\_->Just $ GPS)] [HH.text "Position"],
                HH.p [] [HH.text $ show state.position]]

-- | Handles all actions for the login component
handleAction ∷ ∀ r o m . MonadAff m
            ⇒ ManageNavigation m
            => MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () o m Unit -- ^ The handled action
      
-- | Submit => Whenever the Position button is pressed, it will get the GPS so it can be displayed
handleAction GPS = do
 loc <- asks _.geo
 case loc of
   Just x -> do
      pos <- H.liftAff $ getCurrentPosition defaultOptions x
      H.modify_ (\st -> st { position = Just pos})
      H.liftEffect $ log $ "Position: " <> show pos
   Nothing -> do
      H.liftEffect $ log $ "No Position"