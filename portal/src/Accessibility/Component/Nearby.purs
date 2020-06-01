-- |
-- | The nearby component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessibility.Component.Nearby (component, Slot(..)) where

-- Language imports
import Prelude

-- Data imports
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
import Halogen.Query.EventSource as HQE

-- Web imports
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

import Web.Event.Event as E

import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.DOM.Element (toEventTarget)

-- Our own imports
import Accessibility.FFI.OpenLayers
  ( OLMap
  , OLGeolocation
  , OLLayer
  , createMap
  , createPOILayer
  , removeTarget
  , setCenter
  , addGeolocationToMap
  , setTracking
  , getCoordinate
  , _getCoordinate
  , removeLayerFromMap
  , addLayerToMap
  , setTestMode
  , POI
  , POIType(..))
import Accessibility.Data.Route (Page(..)) as ADR
import Accessibility.Component.HTML.Utils (css, style)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Item (class ManageItem, queryItems, Item)
import Accessibility.Interface.Entity (class ManageEntity, queryEntities, Entity(..))

-- | Slot type for the Login component
type Slot p = forall q . H.Slot q Void p

-- | State for the component
type State =  { alert           ::Maybe String            -- ^ The alert text
                , subscription  ::Maybe H.SubscriptionId  -- ^The add item button subscription
                , geo           ::Maybe OLGeolocation     -- ^ The GeoLocator device
                , map           ::Maybe OLMap             -- ^ The Map on the page
                , poi           ::Maybe OLLayer           -- ^ The POI Layer
                , mock          ::Boolean                 -- ^ Mock of GPS, always at Storgaten, Sundsvall
                , distance      ::Number                  -- ^ The max search distance
              }

-- | Initial state is no logged in user
initialState :: forall i. i -- ^ Initial input
  -> State                  -- ^ The state
initialState _ =  { alert           : Nothing
                    , subscription  : Nothing
                    , geo           : Nothing
                    , map           : Nothing
                    , poi           : Nothing
                    , mock          : false
                    , distance : 300.0
                  }

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Update
  | Center
  | AddItem
  | Mock Boolean
  | Add

-- | Convert an Item to a POI
itemToPOI :: Item -- ^The item to be converted
          -> POI  -- ^The POI
itemToPOI i = { latitude    : i.latitude
                , longitude : i.longitude
                , name      : i.name
                , type      : Point}

-- | Convert an Entity to a POI
entityToPOI :: Entity -- ^The entity to be converted
            -> POI    -- ^The POI
entityToPOI (Entity e) = {
  latitude: fromMaybe 0.0 $ e.location.value.coordinates!!1, 
  longitude: fromMaybe 0.0 $ e.location.value.coordinates!!0,
  name: fromMaybe "?" $ (entityNameTemperature e) <|> (entityNameSnowHeight e),
  type: Weather}

  where

    entityNameTemperature en = (flip append "C") <$> (((append "T:") <<< show <<< _.value) <$> en.temperature)
    entityNameSnowHeight  en  = (flip append "mm") <$> (((append "d:") <<< show <<< _.value) <$> en.snowHeight)

-- | The component definition
component :: forall r q i o m . MonadAff m
          => ManageNavigation m
          => MonadAsk r m
          => ManageEntity m
          => ManageItem m
          => H.Component HH.HTML q i o m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
      initialize = Just Initialize,
      finalize = Just Finalize
     }
    }

nearbyAlert ::forall p i . Maybe String
            -> HH.HTML p i
nearbyAlert (Just t) = HH.div [css "alert alert-danger"] [HH.text $ t]
nearbyAlert Nothing = HH.div [] []

-- | Render the nearby page
render  :: forall m . MonadAff m
        => State                        -- ^ The state to render
        -> H.ComponentHTML Action () m  -- ^ The components HTML
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
handleAction  :: forall r o m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Action                               -- ^ The action to handle
              -> H.HalogenM State Action () o m Unit  -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize Nearby component"
  state <- H.get

  -- Create the map and add a geolocation and start tracking
  olmap <- H.liftEffect $ createMap "map" 0.0 0.0 18
  g <- H.liftEffect $ join <$> (sequence $ addGeolocationToMap <$> olmap)
  H.liftEffect $ sequence_ $ setTracking <$> g <*> (Just true)

  -- Get the weather data from the IoT Hb and add it to our POI layer
  pos <- H.liftAff $ sequence $ _getCoordinate <$> g 
  entities <- queryEntities "WeatherObserved"

  -- Get all items within our radius
  items <- queryItems {
    longitude : join $ _.longitude <$> pos, 
    latitude: join $ _.latitude <$> pos, 
    distance: Just state.distance,
    limit: Nothing,
    text: Nothing }

  -- Merge the two data sources into one layer
  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> ((map (map itemToPOI) items) <> (map (map entityToPOI) entities))

  -- Add the layer to the map and center it around our location
  H.liftEffect do
    sequence_ $ addLayerToMap <$> olmap <*> layer
    sequence_ $ setCenter <$> olmap <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)  

  -- Add a listener to the add item button on the map
  element <- H.liftEffect $ 
    (toParentNode <$> (window >>= document)) >>=
    (querySelector (QuerySelector "#add-item"))  
  s <- sequence $ subscribe <$> element

  -- Update the state
  H.put state { poi = layer
    , subscription = s
    , map = olmap
    , geo = g
    , alert = maybe (Just "Unable to get a geolocation device") (const Nothing) g}

  where

    -- Subscribe to a click event for a button
    subscribe e = H.subscribe do
      HQE.eventListenerEventSource
        (E.EventType "click")
        (toEventTarget e)
        (const (Just AddItem))

-- | Add an item to the database based on the current position
handleAction AddItem = do
  H.liftEffect $ log $ "Add and item button clicked"
  state <- H.get
  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
  sequence_ $ gotoPage <$> (ADR.AddPoint <$> (join $ _.latitude <$> pos) <*> (join $ _.longitude <$> pos))

-- | Finalize action, clean up the map
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get

  -- Remove the subscription
  sequence_ $ H.unsubscribe <$> state.subscription

  -- Remove the tracking, layers and the target of the map
  H.liftEffect $ do
    sequence_ $ (flip setTracking false) <$> state.geo
    sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
    sequence_ $ removeTarget <$> state.map

  -- Update the state
  H.put state { map = Nothing, geo = Nothing, alert = Nothing }

-- | Find the items and create a layer and display it
handleAction Update = do
  H.liftEffect $ log "Make an items update"
  state <- H.get

  -- Get the current position
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
  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> ((map (map itemToPOI) items) <> (map (map entityToPOI) entities))
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
  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> ((map (map itemToPOI) items) <> (map (map entityToPOI) entities))
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

