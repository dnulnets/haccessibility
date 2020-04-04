-- |
-- | The nearby component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessability.Component.Nearby where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..))
import Data.Nullable (toMaybe)

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (try)

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
import Web.OL.Map

-- Our own stuff
import Accessability.Component.HTML.Utils (css, style)
import Accessability.Interface.Navigate (class ManageNavigation)

-- | Slot type for the Login component
type Slot p = ∀ q . H.Slot q Void p

-- | State for the component
type State = {  alert::Maybe String,   -- ^ The alert text
                position::Maybe Position,
                map::Maybe OLMap}  -- ^ The GPS position of the user

-- | Initial state is no logged in user
initialState ∷ ∀ i. i   -- ^ Initial input
  → State               -- ^ The state
initialState _ = { alert : Nothing,
                   position : Nothing,
                   map : Nothing }

-- | Internal form actions
data Action = Initialize | Finalize | GPS

-- | The component definition
component ∷ ∀ r q i o m . MonadAff m
            ⇒ ManageNavigation m
            ⇒ MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
            ⇒ H.Component HH.HTML q i o m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
      initialize = Just Initialize,
      finalize = Just Finalize
     }
    }

nearbyAlert::forall p i . Maybe String -> HH.HTML p i
nearbyAlert t = HH.b [css "", style $ ("color:red;visibility:" <> (maybe "hidden" (\_->"visible") t))] 
  [HH.text $ fromMaybe "" t]

-- | Render the nearby page
render ∷ ∀ m . MonadAff m ⇒ State -- ^ The state to render
  → H.ComponentHTML Action () m   -- ^ The components HTML
render state = HH.div
               [css "container-fluid"]
               [HH.div [HP.id_ "map"][], HH.div [css "row"]
                 [ HH.div [css "col-sm"] [HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton, HE.onClick (\_->Just $ GPS)] [HH.text "Update position"]],
                    HH.div [css "col-sm"] [HH.label [HP.for "longitude"] [HH.text "Longitude"],
                      HH.input [HP.value (fromMaybe "?" (show <$> ((_.coords.longitude) <$> state.position))), HP.id_ "longitude", HP.type_ HP.InputText,
                        HPA.label "longitude", HP.placeholder "Longitude"]],
                    HH.div [css "col-sm"] [HH.label [HP.for "latitude"] [HH.text "Latitude"],
                      HH.input [HP.value (fromMaybe "?" (show <$> ((_.coords.latitude) <$> state.position))), HP.id_ "latitude", HP.type_ HP.InputText,
                        HPA.label "latitude", HP.placeholder "Latitude"]]],HH.text $ show state.position]

-- | Handles all actions for the login component
handleAction ∷ ∀ r o m . MonadAff m
            ⇒ ManageNavigation m
            => MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () o m Unit -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
 loc <- asks _.geo
 H.liftEffect $ log "Initialize Nearby Component"
 case loc of
   Just x -> do
      pos <- H.liftAff $ try $ getCurrentPosition defaultOptions x
      case pos of
        Right p -> do
          H.modify_ (\st -> st { position = Just p})
          H.liftEffect $ log $ "Position: " <> show p
          olmap <- H.liftEffect $ toMaybe <$> (createMap "map" p.coords.longitude p.coords.latitude 10)
          state <- H.get
          H.put state {map = olmap}
        Left e -> do
          H.liftEffect $ log $ "Position error: " <> (show e)
          olmap <- H.liftEffect $ toMaybe <$> (createMap "map" 17.3063 62.39129 10)
          state <- H.get
          H.put state {map = olmap}
   Nothing -> do
      H.liftEffect $ log $ "No Position device"
      olmap <- H.liftEffect $ toMaybe <$> (createMap "map" 17.3063 62.39129 10)
      state <- H.get
      H.put state {map = olmap}

-- | Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get
  case state.map of
    Just x -> do
      H.liftEffect $ removeTarget x
      H.put state { map = Nothing }
      H.liftEffect $ log "Removed target"
    Nothing -> do
      H.liftEffect $ log "Nothing to remove"

-- | GPS Update position set
handleAction GPS = do
 loc <- asks _.geo
 state <- H.get
 case state.map of
  Just map -> do
    case loc of
      Just gps -> do
        pos <- H.liftAff $ try $ getCurrentPosition defaultOptions gps
        case pos of
          Right p -> do
              H.liftEffect $ log $ "Got position " <> show p
              H.liftEffect $ setCenter map p.coords.longitude p.coords.latitude
          Left e -> do
              H.liftEffect $ setCenter map 20.95279 64.75067
              H.liftEffect $ log $ "Unable to get position " <> show e
      Nothing -> do
        H.liftEffect $ log "Unable to get position, no gps"
  Nothing -> do
    H.liftEffect $ log "No map available"
