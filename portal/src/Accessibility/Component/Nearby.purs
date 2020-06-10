-- |
-- | The nearby component
-- |
-- | Written by Tomas Stenlund, Sundsvall,Sweden (c) 2020
-- |
module Accessibility.Component.Nearby (component, Slot(..)) where

-- Language imports
import Prelude

-- Data imports
import Data.Array((!!), catMaybes, length, head)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foldable (sequence_, oneOfMap)
import Data.Traversable (sequence)
import Data.Unfoldable (fromMaybe) as DU
import Data.Nullable

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Alt ((<|>))

-- Effects
import Effect (foreachE)
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
  , addInteraction
  , POI
  , POIType(..))

import OpenLayers.Interaction.Select as Select
import OpenLayers.Feature as Feature
import OpenLayers.Source.OSM as OSM
import OpenLayers.Layer.Tile as Tile
import OpenLayers.Proj as Proj
import OpenLayers.View as View
import OpenLayers.Map as Map
import OpenLayers.Geolocation as Geolocation
import OpenLayers.Style.Style as Style
import OpenLayers.Style.Fill as Fill
import OpenLayers.Style.Circle as Circle
import OpenLayers.Style.Stroke as Stroke
import OpenLayers.Geom.Point as Point
import OpenLayers.Layer.Vector as VectorLayer
import OpenLayers.Source.Vector as VectorSource

import Accessibility.Data.Route (Page(..)) as ADR
import Accessibility.Component.HTML.Utils (css, style)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Item (class ManageItem, queryItems, Item)
import Accessibility.Interface.Entity (class ManageEntity, queryEntities, Entity(..))

-- | Slot type for the Login component
type Slot p = forall q . H.Slot q Void p

-- | State for the component
type State =  { alert           ::Maybe String            -- ^ The alert text
                , subscription  ::Array H.SubscriptionId  -- ^The add item button subscription
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
                    , subscription  : []
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
  | FeatureSelect Select.SelectEvent
  | Mock Boolean
  | Add
  | GPSError
  | GPSPosition Geolocation.Geolocation Feature.Feature
  | GPSAccuracy

-- | Convert an Item to a POI
itemToPOI :: Item -- ^The item to be converted
          -> POI  -- ^The POI
itemToPOI i = { id          : fromMaybe "" i.id
                , latitude  : i.latitude
                , longitude : i.longitude
                , name      : i.name
                , type      : Point}

-- | Convert an Entity to a POI
entityToPOI :: Entity -- ^The entity to be converted
            -> POI    -- ^The POI
entityToPOI (Entity e) = {
  id: "",
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
               [css "d-flex flex-column ha-nearby"]
               [HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][nearbyAlert state.alert]],
                HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.h2 [][HH.text "Point of interests"]]],
                HH.div [css "row flex-grow-1 ha-nearby-map"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "ha-map"][]]]
                ]

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

  -- This is my playground for the OpenLayers conversion
  osm <- H.liftEffect $ OSM.create {}

  tile <- H.liftEffect $ sequence $ Tile.create <$> ((\o->{source: o }) <$> osm )

  view <- H.liftEffect $ View.create { projection: Proj.epsg_3857 
                                      , center: Proj.fromLonLat [0.0, 0.0] (Just Proj.epsg_3857)
                                      , zoom: 18 }

  hamap <- H.liftEffect $ sequence $ Map.create <$> ((\t v -> {
      target: "ha-map"
      , layers: [ t ]
      , view: v
    }) <$> (join tile) <*> view)

  -- Get the geolocation device
  mgeo <- H.liftEffect $ Geolocation.create {
      trackingOptions: { enableHighAccuracy: true}
      , projection: Proj.epsg_3857
    }

  -- Add listeners to the geolocation device
  case mgeo of
    Just geo -> do

      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onError (\_ -> HQE.emit emitter GPSError) geo
        pure (HQE.Finalizer (Geolocation.unError key geo))

      -- Create the GPS Position Feature
      pfill <- H.liftEffect $ Fill.create { color: "#3399CC" }
      pstroke <- H.liftEffect $ Stroke.create { color: "#fff", width: 2}
      pcircle <- H.liftEffect $ Circle.create {
        radius: 6
        , fill: fromMaybe null $ notNull <$> pfill
        , stroke: fromMaybe null $ notNull <$> pstroke
        }
      pstyle <- H.liftEffect $ Style.create {image: fromMaybe null $ notNull <$> pcircle}
      pfeat <- H.liftEffect $ Feature.create {}
      H.liftEffect $ sequence_ $ (Feature.setStyle pstyle) <$> pfeat
      psvector <- H.liftEffect $ VectorSource.create {features: (DU.fromMaybe pfeat)::(Array Feature.Feature)}
      plvector <- H.liftEffect $ VectorLayer.create { source: fromMaybe null $ notNull <$> psvector }
      H.liftEffect $ sequence_ $ Map.addLayer <$> plvector <*> (join hamap)

      -- Event handler for GPS Position
      case pfeat of
        Just feat -> do
          void $ H.subscribe $ HQE.effectEventSource \emitter -> do
            key <- Geolocation.onChangePosition (\_ -> HQE.emit emitter (GPSPosition geo feat)) geo
            pure (HQE.Finalizer (Geolocation.unChangePosition key geo))
        Nothing -> do
          H.liftEffect $ log "No GPS Position handler"

      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangeAccuracyGeometry (\_ -> HQE.emit emitter GPSAccuracy) geo
        pure (HQE.Finalizer (Geolocation.unChangeAccuracyGeometry key geo))

      -- Turn on the geo location device
      H.liftEffect $ Geolocation.setTracking true geo

    Nothing -> do
      H.liftEffect $ log "No error subscription"

  -- Create the map and add a geolocation and start tracking
  -- olmap <- H.liftEffect $ createMap "ha-map" 0.0 0.0 18
  olmap <- pure Nothing
  g <- H.liftEffect $ join <$> (sequence $ addGeolocationToMap <$> olmap)
  H.liftEffect $ sequence_ $ setTracking <$> g <*> (Just true)

  -- Get te position from the GPS
  pos <- H.liftAff $ sequence $ _getCoordinate <$> g

  -- Get the weather data from the IoT Hb and add it to our POI layer
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
    -- sequence_ $ addLayerToMap <$> olmap <*> layer
    sequence_ $ setCenter <$> olmap <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)  

  -- Add a listener to the add item button on the map
  eadd <- H.liftEffect $ 
    (toParentNode <$> (window >>= document)) >>=
    (querySelector (QuerySelector "#map-add-item"))  
  sadd <- sequence $ (subscribe AddItem) <$> eadd

  -- Add a listener to the refresh item button on the map
  eupd <- H.liftEffect $ 
    (toParentNode <$> (window >>= document)) >>=
    (querySelector (QuerySelector "#map-refresh"))  
  supd <- sequence $ (subscribe Update) <$> eupd

  -- Add a listener to the refresh item button on the map
  ecen <- H.liftEffect $ 
    (toParentNode <$> (window >>= document)) >>=
    (querySelector (QuerySelector "#map-center"))  
  scen <- sequence $ (subscribe Center) <$> ecen

  -- Subscribe for feature selects on the map
  s <- H.liftEffect $ Select.create $ Just {multi: false}
  sfeat <- H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Select.onSelect (\e -> HQE.emit emitter (FeatureSelect e)) s
        pure (HQE.Finalizer (Select.unSelect key s))
  H.liftEffect $ sequence_ $ addInteraction <$> olmap <*> (Just s)

  -- Update the state
  H.put state { poi = layer
    , subscription = (catMaybes [sadd, supd, scen]) <> [sfeat]
    , map = olmap
    , geo = g
    , alert = maybe (Just "Unable to get a geolocation device") (const Nothing) g}

  where

    -- Subscribe to a click event for a button
    subscribe a e = H.subscribe do
      HQE.eventListenerEventSource
        (E.EventType "click")
        (toEventTarget e)
        (const (Just a))

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

handleAction (FeatureSelect e) = do
  H.liftEffect $ log "Feature selected!"
  H.liftEffect $ log $ "Selected length is " <> (show (length e.selected))
  l <- H.liftEffect $ sequence $ (Feature.get "id") <$> e.selected
  sequence_ $ gotoPage <$> (ADR.Point <$> (head (catMaybes l)) <*> (Just false))

-- | Activate or deactivate the test mode of the mobile
handleAction (Mock b) = do
  state <- H.get
  H.liftEffect do
    log "Test mode on/off"
    sequence_ $ setTracking <$> state.geo <*> (Just $ not b)  
    sequence_ $ (flip setTestMode b) <$> state.map
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
  H.liftEffect do
    sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
    sequence_ $ addLayerToMap <$> state.map <*> layer
    sequence_ $ setCenter <$> state.map <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)
  H.put state { mock = b, poi = layer }

-- | Find the items
handleAction Add = do
  H.liftEffect $ log "Adds a POI to the database"
  state <- H.get
  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
  H.liftEffect $ log $ show pos

-- | GPS Error
handleAction GPSError = do
  H.liftEffect $ log "GPS Error!!!"

-- | GPS Position
handleAction (GPSPosition g f) = do
  pos <- H.liftEffect $ Geolocation.getPosition g
  mp <- H.liftEffect $ sequence $ Point.create <$> pos <*> (Just Nothing)
  H.liftEffect $ Feature.setGeometry (join mp) f
  H.liftEffect $ log $ "GPS Position: " <> (show pos)

-- positionFeature.setGeometry(new olg.Point(olp.fromLonLat([mock_lon, mock_lat], mock_accuracy)));

-- | GPS Accuracy
handleAction GPSAccuracy = do
  H.liftEffect $ log "GPS Accuracy!!!"
