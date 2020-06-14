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
import Data.Tuple (Tuple(..), fst, snd)

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Alt ((<|>))

-- Effects
import Effect (Effect)
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
import OpenLayers.Style.Text as Text
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
                , subscription  ::Array H.SubscriptionId  -- ^ The add item button subscription
                , geo           ::Maybe Geolocation.Geolocation     -- ^ The GeoLocator device
                , map           ::Maybe Map.Map                     -- ^ The Map on the page
                , poi           ::Maybe VectorLayer.Vector          -- ^ The POI Layer
                , distance      ::Number                  -- ^ The max search distance
                , poiStyle      ::Maybe Circle.Circle     -- ^ The POI Style
                , iotStyle      ::Maybe Circle.Circle     -- ^ The IoT Hub Style
              }

-- | Initial state is no logged in user
initialState :: forall i. i -- ^ Initial input
  -> State                  -- ^ The state
initialState _ =  { alert           : Nothing
                    , subscription  : []
                    , geo           : Nothing
                    , map           : Nothing
                    , poi           : Nothing
                    , distance : 300.0
                    , poiStyle : Nothing
                    , iotStyle : Nothing
                  }

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Update
  | Center
  | AddItem
  | FeatureSelect Select.SelectEvent
  | Add
  | GPSError
  | GPSPosition Geolocation.Geolocation Feature.Feature
  | GPSAccuracy Geolocation.Geolocation Feature.Feature
  | GPSCenter Geolocation.Geolocation Map.Map

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

  -- Create the OpenLayers Map items
  hamap <- H.liftEffect $ createNearbyMap
  gps <- createNearbyGPS hamap

  -- Create the styles for the POI and IOT Hub Entities
  style <- H.liftEffect $ do
    olPOIFill <- Fill.create {color: "#32CD32"}
    olIOTFill <- Fill.create {color: "#0080FF"}
    olSYMStroke <- Stroke.create {color: "#000000", width:2}
    olPOIStyle <- Circle.create { radius: 6
      , fill: toNullable olPOIFill
      , stroke: toNullable olSYMStroke }
    olIOTStyle <- Circle.create { radius: 6
      , fill: toNullable olIOTFill
      , stroke: toNullable olSYMStroke }
    olTextStyle <- Text.create {text: "Mandel"
                                , offsetY: 15
                                , font: "12px Calibri, sans-serif" }

    olStyle <- Style.create { image: toNullable olPOIStyle, text: toNullable olTextStyle}
    pure $ olStyle

  -- Create the layer
  void $ sequence_ $ addNearbyPOI <$> hamap <*> style

  -- Add a listener to the add item button on the map
--  eadd <- H.liftEffect $ 
--    (toParentNode <$> (window >>= document)) >>=
--    (querySelector (QuerySelector "#map-add-item"))  
--  sadd <- sequence $ (subscribe AddItem) <$> eadd

  -- Add a listener to the refresh item button on the map
--  eupd <- H.liftEffect $ 
--    (toParentNode <$> (window >>= document)) >>=
--    (querySelector (QuerySelector "#map-refresh"))  
--  supd <- sequence $ (subscribe Update) <$> eupd

  -- Add a listener to the refresh item button on the map
--  ecen <- H.liftEffect $ 
--    (toParentNode <$> (window >>= document)) >>=
--    (querySelector (QuerySelector "#map-center"))  
--  scen <- sequence $ (subscribe Center) <$> ecen

  -- Subscribe for feature selects on the map
--  s <- H.liftEffect $ Select.create $ Just {multi: false}
--  sfeat <- H.subscribe $ HQE.effectEventSource \emitter -> do
--        key <- Select.onSelect (\e -> HQE.emit emitter (FeatureSelect e)) s
--        pure (HQE.Finalizer (Select.unSelect key s))
--  H.liftEffect $ sequence_ $ addInteraction <$> olmap <*> (Just s)

  -- Update the state
  H.put state { subscription = [] -- (catMaybes [sadd, supd, scen]) <> [sfeat]
                , map = hamap
                , geo = gps
                , alert = maybe (Just "Unable to get a geolocation device") (const Nothing) gps}

--  where

    -- Subscribe to a click event for a button
--    subscribe a e = H.subscribe do
--      HQE.eventListenerEventSource
--        (E.EventType "click")
--        (toEventTarget e)
--        (const (Just a))

-- | Add an item to the database based on the current position
handleAction AddItem = do
  H.liftEffect $ log $ "Add and item button clicked"
--  state <- H.get
--  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
--  sequence_ $ gotoPage <$> (ADR.AddPoint <$> (join $ _.latitude <$> pos) <*> (join $ _.longitude <$> pos))

-- | Finalize action, clean up the map
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get

  -- Remove the subscription
  sequence_ $ H.unsubscribe <$> state.subscription

  -- Remove the tracking, layers and the target of the map
--  H.liftEffect $ do
--    sequence_ $ (flip setTracking false) <$> state.geo
--    sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
--    sequence_ $ removeTarget <$> state.map

  -- Update the state
  H.put state { map = Nothing, geo = Nothing, alert = Nothing }

-- | Find the items and create a layer and display it
handleAction Update = do
  H.liftEffect $ log "Make an items update"
--  state <- H.get

  -- Get the current position
--  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
--  entities <- queryEntities "WeatherObserved"
--  H.liftEffect $ log $ show entities  
--  items <- queryItems {
--    longitude : join $ _.longitude <$> pos, 
--    latitude: join $ _.latitude <$> pos, 
--    distance: Just state.distance,
--    limit: Nothing,
--    text: Nothing }
--  H.liftEffect $ sequence_ $ removeLayerFromMap <$> state.map <*> state.poi
--  layer <- H.liftEffect $ sequence $ createPOILayer <$> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos) <*> (Just (state.distance*2.0)) <*> ((map (map itemToPOI) items) <> (map (map entityToPOI) entities))
--  H.liftEffect $ sequence_ $ addLayerToMap <$> state.map <*> layer
--  H.put state { poi = layer }

-- | Find the items
handleAction Center = do
  H.liftEffect $ log "Center the map around the GPS location"
--  state <- H.get
--  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
--  H.liftEffect $ sequence_ $ setCenter <$> state.map <*> (join $ _.longitude <$> pos) <*> (join $ _.latitude <$> pos)

handleAction (FeatureSelect e) = do
  H.liftEffect $ log "Feature selected!"
  H.liftEffect $ log $ "Selected length is " <> (show (length e.selected))
  l <- H.liftEffect $ sequence $ (Feature.get "id") <$> e.selected
  sequence_ $ gotoPage <$> (ADR.Point <$> (head (catMaybes l)) <*> (Just false))

-- | Find the items
handleAction Add = do
  H.liftEffect $ log "Adds a POI to the database"
--  state <- H.get
--  pos <- H.liftEffect $ sequence $ getCoordinate <$> state.geo
--  H.liftEffect $ log $ show pos

-- | GPS Error - Error in the geolocation device
handleAction GPSError = H.liftEffect $ do
  log "GPS Error!!!"

-- | GPS Position - Position the current location on the map
handleAction (GPSPosition geo feature) = H.liftEffect $ do
  pos <- Geolocation.getPosition geo
  point <- join <$> (sequence $ Point.create <$> pos <*> (Just Nothing))
  Feature.setGeometry point feature

-- | GPS Accuracy - Position the accuracy polygon on the map
handleAction (GPSAccuracy geo feature) = H.liftEffect $ do
  polygon <- Geolocation.getAccuracyGeometry geo
  Feature.setGeometry polygon feature

-- | GPS Center - Center the map based on geolocation
handleAction (GPSCenter geo map) = H.liftEffect $ do
  pos <- Geolocation.getPosition geo
  mv <- Map.getView map
  sequence_ $ View.setCenter <$> pos <*> mv

--
-- Creates the map and attaches openstreetmap as a source
--
createNearbyMap::Effect (Maybe Map.Map)
createNearbyMap = do
  -- Use OpenStreetMap as a source
  osm <- OSM.create {}
  tile <- join <$> (sequence $ Tile.create <$> ((\o->{source: o }) <$> osm ))

  -- Create the map and view
  view <- View.create { projection: Proj.epsg_3857 
                        , center: Proj.fromLonLat [0.0, 0.0] (Just Proj.epsg_3857)
                        , zoom: 18 }
  join <$> (sequence $ Map.create <$> ((\t v -> {
      target: "ha-map"
      , layers: [ t ]
      , view: v
    }) <$> tile <*> view))

--
-- Create the GPS and add all handlers
--
createNearbyGPS:: forall r o m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Maybe Map.Map
              -> H.HalogenM State Action () o m (Maybe Geolocation.Geolocation)
createNearbyGPS Nothing = pure Nothing
createNearbyGPS (Just map) = do
  -- Create the GPS device
  mgeo <- H.liftEffect $ Geolocation.create {
      trackingOptions: { enableHighAccuracy: true}
      , projection: Proj.epsg_3857
    }
  -- Set up all handlers and features
  setupNearbyGPS mgeo
  pure mgeo
  where

    setupNearbyGPSPositionHandler _ Nothing = H.liftEffect $ do
      log "No GPS Position feature"

    setupNearbyGPSPositionHandler geo (Just feat) = do    
      -- Change of Position
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangePosition (\_ -> HQE.emit emitter (GPSPosition geo feat)) geo
        pure (HQE.Finalizer (Geolocation.unChangePosition key geo))

    setupNearbyGPSAccuracyHandler _ Nothing = H.liftEffect $ do
      log "No GPS Accuracy feature"

    setupNearbyGPSAccuracyHandler geo (Just feat) = do
      -- Change of Accuracy
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangeAccuracyGeometry (\_ -> HQE.emit emitter (GPSAccuracy geo feat)) geo
        pure (HQE.Finalizer (Geolocation.unChangeAccuracyGeometry key geo))

    setupNearbyGPS Nothing = H.liftEffect $ do
      log "No GPS available"

    setupNearbyGPS (Just geo) = do

      -- Create the GPS Error handler
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onError (\_ -> HQE.emit emitter GPSError) geo
        pure (HQE.Finalizer (Geolocation.unError key geo))

      -- Create the GPS Position Feature, a dot with a circle
      mfeat <- H.liftEffect $ do
        pfill <- Fill.create { color: "#3399CC" }
        pstroke <- Stroke.create { color: "#fff", width: 2}
        pcircle <- Circle.create {
          radius: 6
          , fill: fromMaybe null $ notNull <$> pfill
          , stroke: fromMaybe null $ notNull <$> pstroke
          }
        pstyle <- Style.create {image: fromMaybe null $ notNull <$> pcircle}
        pfeat <- Feature.create {}
        pafeat <- Feature.create {}
        sequence_ $ (Feature.setStyle pstyle) <$> pfeat
        psvector <- VectorSource.create {features: catMaybes [pfeat, pafeat]}
        plvector <- VectorLayer.create { source: fromMaybe null $ notNull <$> psvector }
        sequence_ $ Map.addLayer <$> plvector <*> (Just map)
        pure $ Tuple pfeat pafeat

      -- Event handlers for the GPS Position
      setupNearbyGPSPositionHandler geo $ fst mfeat
      setupNearbyGPSAccuracyHandler geo $ snd mfeat

      -- Turn on the geo location device
      H.liftEffect $ Geolocation.setTracking true geo

      -- Get the current position and position the map
      void $ H.subscribe' $ \_ -> (HQE.effectEventSource \emitter -> do
        key <- Geolocation.onceChangePosition (\_ -> HQE.emit emitter (GPSCenter geo map)) geo
        pure (HQE.Finalizer (Geolocation.unChangePosition key geo)))

--
-- Create the layer and add our POI and data  from the IoTHub
--
addNearbyPOI:: forall r o m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Map.Map
              -> Style.Style
              -> H.HalogenM State Action () o m Unit
addNearbyPOI map style = do

  -- Get the weather data from the IoT Hb and our own backend
  entities <- queryEntities "WeatherObserved"

  -- Get all items within our radius
  items <- (fromMaybe []) <$> queryItems {longitude : Nothing
                                          , latitude: Nothing
                                          , distance: Nothing
                                          , limit: Nothing
                                          , text: Nothing }
  
  -- Create the features and vectors
  H.liftEffect $ log $ "Creating POI"
  flist <- H.liftEffect $ sequence $ toFeature <$> items
  H.liftEffect $ log $ show $ flist
  vs <- H.liftEffect $ VectorSource.create { features: catMaybes flist }
  vl <- H.liftEffect $ VectorLayer.create { source: toNullable vs}
  H.liftEffect $ sequence_ $ VectorLayer.setStyle <$> (Just (VectorLayer.Style style)) <*> vl
  H.liftEffect $ sequence_ $ Map.addLayer <$> vl <*> (Just map)

  where

    toFeature::Item->Effect (Maybe Feature.Feature)
    toFeature i = do
      point <- Point.create 
        (Proj.fromLonLat [i.longitude, i.latitude] (Just Proj.epsg_3857))
        Nothing
      Feature.create {name: i.name
                      , id: fromMaybe "" i.id
                      , type: 1
                      , geometry: toNullable point }
