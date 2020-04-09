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
import Web.OL.Map (OLMap,
  OLGeolocation,
  createMap,
  removeTarget,
  setCenter,
  addGeolocationToMap,
  setTracking)

-- Our own stuff
import Accessability.Component.HTML.Utils (css, style)
import Accessability.Interface.Navigate (class ManageNavigation)

-- | Slot type for the Login component
type Slot p = ∀ q . H.Slot q Void p

-- | State for the component
type State = {  alert::Maybe String,   -- ^ The alert text
                geo::Maybe OLGeolocation,
                map::Maybe OLMap}  -- ^ The GPS position of the user

-- | Initial state is no logged in user
initialState ∷ ∀ i. i   -- ^ Initial input
  → State               -- ^ The state
initialState _ = { alert : Nothing,
                   geo : Nothing,
                   map : Nothing }

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Tracking Boolean

-- | The component definition
component ∷ ∀ r q i o m . MonadAff m
            ⇒ ManageNavigation m
            ⇒ MonadAsk r m
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
nearbyAlert (Just t) = HH.div [css "alert alert-danger"] [HH.text $ t]
nearbyAlert Nothing = HH.div [] []

-- | Render the nearby page
render ∷ ∀ m . MonadAff m ⇒ State -- ^ The state to render
  → H.ComponentHTML Action () m   -- ^ The components HTML
render state = HH.div
               [css "container-fluid"]
               [HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][nearbyAlert state.alert]],
                HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "map"][]]],
                HH.div [css "row"]
                 [  HH.div [css "col-xs-2 col-sm-2"] [ HH.div [css "form-check"] [
                      HH.input [css "form-check-input", HP.id_ "update", HP.type_ HP.InputCheckbox, HE.onChecked (\b->Just $ Tracking b)],
                      HH.label [css "form-check-label", HP.for "update"] [HH.text "Continuous update"]
                      ]
                    ],
                    HH.div [css "col-xs-12 col-sm-12"] [HH.text $ show state.alert]]]

-- | Handles all actions for the login component
handleAction ∷ ∀ r o m . MonadAff m
            ⇒ ManageNavigation m
            => MonadAsk r m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () o m Unit -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
  state <- H.get
  H.liftEffect $ log "Initialize Nearby Component"
  olmap <- H.liftEffect $ toMaybe <$> (createMap "map" 0.0 0.0 10)
  case olmap of
    Just map -> do
      g <- H.liftEffect $ toMaybe <$> (addGeolocationToMap map)
      H.put state {map = olmap, geo = g, alert = Nothing}
      H.liftEffect $ log "Got a geo location"
    Nothing -> do
      H.liftEffect $ log "Failed to get a map"
      H.put state {map = Nothing, geo = Nothing, alert = Just "Unable to create map"}

-- | Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get
  case state.map of
    Just x -> do
      H.liftEffect $ removeTarget x
      H.liftEffect $ log "Removed target"
    Nothing -> do
      H.liftEffect $ log "Nothing to remove"
  H.put state { map = Nothing, geo = Nothing, alert = Nothing }

-- | GPS Update position set
handleAction (Tracking b) = do
 state <- H.get
 case state.geo of
  Just g -> do
    H.liftEffect $ setTracking g b
    H.liftEffect $ log "Geo location tracking pressed"
  Nothing -> do
    H.liftEffect $ log "No geo location device available"