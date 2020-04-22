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
import Data.Foldable (sequence_)
import Data.Traversable (sequence, traverse)

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Monad.Reader (asks)
import Control.Monad.Error.Class (try)
import Control.Monad (join)

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
  Coordinates(..),
  createMap,
  removeTarget,
  setCenter,
  addGeolocationToMap,
  setTracking,
  getCoordinates,
  debugWrite)

-- Our own stuff
import Accessability.Component.HTML.Utils (css, style)
import Accessability.Interface.Navigate (class ManageNavigation)
import Accessability.Interface.Item (class ManageItem, queryItems, QueryItems(..), Item(..))

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
  | Lookup
  | Center

-- | The component definition
component ∷ ∀ r q i o m . MonadAff m
            ⇒ ManageNavigation m
            ⇒ MonadAsk r m
            => ManageItem m
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
                HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.h2 [][HH.text "Point of interests"]]],
                HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "map"][]]],
                HH.div [css "row"]
                 [  HH.div [css "col-xs-6 col-sm-2"] [
                      HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton, HE.onClick \_ -> Just Lookup ] [HH.text "Lookup"]
                    ],
                    HH.div [css "col-xs-6 col-sm-2"] [ 
                      HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton, HE.onClick \_ -> Just Center ] [HH.text "Center"]
                    ],
                    HH.div [css "col-sm-8"] [ ]
                    ]]

-- | Handles all actions for the login component
handleAction ∷ ∀ r o m . MonadAff m
            ⇒ ManageNavigation m
            => ManageItem m
            => MonadAsk r m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () o m Unit -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
  state <- H.get
  H.liftEffect $ log "Initialize Nearby Component"
  olmap <- H.liftEffect $ toMaybe <$> (createMap "map" 0.0 0.0 18)
  case olmap of
    Just map -> do
      g <- H.liftEffect $ toMaybe <$> (addGeolocationToMap map)
      H.put state {map = olmap, geo = g, alert = Nothing}
      H.liftEffect $ sequence_ $ setTracking <$> g <*> (Just true)
      H.liftEffect $ log "Got a geo location"
    Nothing -> do
      H.liftEffect $ log "Failed to get a map"
      H.put state {map = Nothing, geo = Nothing, alert = Just "Unable to create map"}

-- | Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get
  H.liftEffect $ sequence_ $ setTracking <$> state.geo <*> (Just false)
  H.liftEffect $ sequence_ $ removeTarget <$> state.map
  H.put state { map = Nothing, geo = Nothing, alert = Nothing }

-- | Find the items
handleAction Lookup = do
  H.liftEffect $ log "Make an items lookup"
  state <- H.get
  tmp <- H.liftEffect $ traverse getCoordinates state.geo
  items <- queryItems {
    longitude : join $ _.longitude <$> tmp, 
    latitude: join $ _.latitude <$> tmp, 
    distance: Just 200.0,
    limit: Nothing,
    text: Nothing }
  H.liftEffect $ log $ show items

-- | Find the items
handleAction Center = do
  H.liftEffect $ log "Center the map around the GPS location"
  state <- H.get
  tmp <- H.liftEffect $ traverse getCoordinates state.geo
  H.liftEffect $ sequence_ $ setCenter <$> state.map <*> (join $ _.longitude <$> tmp) <*> (join $ _.latitude <$> tmp)
