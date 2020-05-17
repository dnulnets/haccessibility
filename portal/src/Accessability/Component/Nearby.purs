-- |
-- | The nearby component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessability.Component.Nearby where

-- Language imports
import Prelude
import Data.Array((!!))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foldable (sequence_)
import Data.Traversable (sequence)

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Alt ((<|>))

-- Effects
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

-- Web imports
import Web.OL.Map (OLMap,
  OLGeolocation,
  OLLayer,
  createMap,
  createPOILayer,
  removeTarget,
  setCenter,
  addGeolocationToMap,
  setTracking,
  getCoordinate,
  _getCoordinate,
  removeLayerFromMap,
  addLayerToMap,
  setTestMode,
  POI, POIType(..))

-- Our own stuff
import Accessability.Component.HTML.Utils (css, style)
import Accessability.Interface.Navigate (class ManageNavigation)
import Accessability.Interface.Item (class ManageItem, queryItems, Item)
import Accessability.Interface.Entity (class ManageEntity, queryEntities, Entity(..))

-- | Slot type for the Login component
type Slot p = ∀ q . H.Slot q Void p

-- | State for the component
type State = {  alert::Maybe String         -- ^ The alert text
                , geo::Maybe OLGeolocation  -- ^ The GeoLocator device
                , map::Maybe OLMap          -- ^ The Map on the page
                , poi::Maybe OLLayer        -- ^ The POI Layer
                , mock::Boolean             -- ^ Mock of GPS, always at Storgaten, Sundsvall
                , distance::Number }        -- ^ The max search distance

-- | Initial state is no logged in user
initialState ∷ ∀ i. i   -- ^ Initial input
  → State               -- ^ The state
initialState _ = { alert : Nothing,
                   geo : Nothing,
                   map : Nothing,
                   poi : Nothing,
                   mock : false,
                   distance : 300.0 }

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Update
  | Center
  | Mock Boolean
  | Add

-- | Convert an Item to a POI
itemToPOI::Item -- ^The item to be converted
  ->POI         -- ^The POI
itemToPOI i = { latitude: i.latitude,
  longitude: i.longitude,
  name: i.name, type: Point}

-- | Convert an Entity to a POI
entityToPOI::Entity -- ^The entity to be converted
  ->POI             -- ^The POI
entityToPOI (Entity e) = {
  latitude: fromMaybe 0.0 $ e.location.value.coordinates!!1, 
  longitude: fromMaybe 0.0 $ e.location.value.coordinates!!0,
  name: fromMaybe "?" $ (entityNameTemperature e) <|> (entityNameSnowHeight e),
  type: Weather}

  where

    entityNameTemperature en = ((append "T:") <<< show <<< _.value) <$> en.temperature
    entityNameSnowHeight  en  = ((append "d:") <<< show <<< _.value) <$> en.snowHeight

-- | The component definition
component ∷ ∀ r q i o m . MonadAff m
            ⇒ ManageNavigation m
            => MonadAsk r m
            => ManageEntity m
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
                 [  HH.div [css "col-xs-3 col-sm-2"] [
                      HH.button [css "btn btn-lg btn-block btn-warning", style "margin-bottom:5px;", HP.type_ HP.ButtonButton, HE.onClick \_ -> Just Update ] [HH.text "Update"]
                    ],
                    HH.div [css "col-xs-3 col-sm-2"] [ 
                      HH.button [css "btn btn-lg btn-block btn-warning", style "margin-bottom:5px;", HP.type_ HP.ButtonButton, HE.onClick \_ -> Just Center ] [HH.text "Center"]
                    ],
                    HH.div [css "col-xs-3 col-sm-2"] [ 
                      HH.button [css "btn btn-lg btn-block btn-warning", style "margin-bottom:5px;", HP.type_ HP.ButtonButton, HE.onClick \_ -> Just Add ] [HH.text "Add"]
                    ],
                    HH.div [css "col-xs-3 col-sm-2"] [ 
                      HH.input [css "form-check-input", style "margin-bottom:5px;", HP.type_ HP.InputCheckbox, HE.onChecked \b -> Just (Mock b)],                      
                      HH.label [css "form-check-label"] [HH.text "Test mode"]
                    ],                    
                    HH.div [css "col-sm-4"] [ ]
                    ]]

-- | Handles all actions for the login component
handleAction ∷ ∀ r o m . MonadAff m
            ⇒ ManageNavigation m
            => ManageEntity m
            => ManageItem m
            => MonadAsk r m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () o m Unit -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
  state <- H.get
  H.liftEffect $ log "Initialize Nearby Component"
  olmap <- H.liftEffect $ createMap "map" 0.0 0.0 18
  g <- H.liftEffect $ join <$> (sequence $ addGeolocationToMap <$> olmap)
  H.liftEffect $ sequence_ $ setTracking <$> g <*> (Just true)
  pos <- H.liftAff $ sequence $ _getCoordinate <$> g 
  entities <- queryEntities "WeatherObserved"
  H.liftEffect $ log $ show entities
  items <- queryItems {
    longitude : join $ _.longitude <$> pos, 
    latitude: join $ _.latitude <$> pos, 
    distance: Just state.distance,
    limit: Nothing,
    text: Nothing }
--  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> (append <$> (map (map itemToPOI) items) <*> (map (map entityToPOI) entities))
  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> ((map (map itemToPOI) items) <> (map (map entityToPOI) entities))
  H.liftEffect do
    sequence_ $ addLayerToMap <$> olmap <*> layer
    sequence_ $ setCenter <$> olmap <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)  
  H.put state { poi = layer, map = olmap, geo = g, alert = maybe (Just "Unable to get a geolocation device") (const Nothing) g}

-- | Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get
  H.liftEffect $ sequence_ $ (flip setTracking false) <$> state.geo
  H.liftEffect $ sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
  H.liftEffect $ sequence_ $ removeTarget <$> state.map
  H.put state { map = Nothing, geo = Nothing, alert = Nothing }

-- | Find the items and create a layer and display it
handleAction Update = do
  H.liftEffect $ log "Make an items update"
  state <- H.get
  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
  entities <- queryEntities "WeatherObserved"
  H.liftEffect $ log $ show entities  
  items <- queryItems {
    longitude : join $ _.longitude <$> pos, 
    latitude: join $ _.latitude <$> pos, 
    distance: Just state.distance,
    limit: Nothing,
    text: Nothing }
  H.liftEffect $ sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> (append <$> (map (map itemToPOI) items) <*> (map (map entityToPOI) entities))
  H.liftEffect $ sequence_ $ addLayerToMap <$> state.map <*> layer
  H.put state { poi = layer }

-- | Find the items
handleAction Center = do
  H.liftEffect $ log "Center the map around the GPS location"
  state <- H.get
  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
  H.liftEffect $ sequence_ $ setCenter <$> state.map <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)

-- | Activate or deactivate the test mode of the mobile
handleAction (Mock b) = do
  state <- H.get
  H.liftEffect $ log "Test mode on/off"
  H.liftEffect $ sequence_ $ setTracking <$> state.geo <*> (Just $ not b)  
  H.liftEffect $ sequence_ $ (flip setTestMode b) <$> state.map
  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
  entities <- queryEntities "WeatherObserved"
  items <- queryItems {
    longitude : join $ _.longitude <$> pos, 
    latitude: join $ _.latitude <$> pos, 
    distance: Just state.distance,
    limit: Nothing,
    text: Nothing }
  H.liftEffect $ sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> (append <$> (map (map itemToPOI) items) <*> (map (map entityToPOI) entities))
  H.liftEffect $ sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
  H.liftEffect $ sequence_ $ addLayerToMap <$> state.map <*> layer
  H.liftEffect $ sequence_ $ setCenter <$> state.map <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)
  H.put state { mock = b, poi = layer }

-- | Find the items
handleAction Add = do
  H.liftEffect $ log "Adds a POI to the database"
  state <- H.get
  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
  H.liftEffect $ log $ show pos

