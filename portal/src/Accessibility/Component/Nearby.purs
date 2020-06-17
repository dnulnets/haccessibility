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
import Data.Foldable (sequence_)
import Data.Traversable (sequence)
import Data.Nullable (Nullable, notNull, null, toNullable)
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
import Halogen.Query.EventSource as HQE

-- DOM and HTML imports
import Web.HTML (window)
import Web.HTML.Window as WHW
import Web.HTML.HTMLDocument as WHHD
import Web.DOM.Document as WDD
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.DOM.Text as WDT

-- Our own imports
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
import OpenLayers.Control.Control as Control
import OpenLayers.Control as Ctrl
import OpenLayers.Collection as Collection

import Accessibility.Data.Route (Page(..)) as ADR
import Accessibility.Component.HTML.Utils (css)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Item (class ManageItem, queryItems, Item)
import Accessibility.Interface.Entity (class ManageEntity, queryEntities, Entity(..))

-- | Slot type for the Login component
type Slot p = forall q . H.Slot q Void p

-- | State for the component
type State =  { alert           ::Maybe String            -- ^ The alert text
                , subscription  ::Array H.SubscriptionId  -- ^ The map button subscriptions
                , geo           ::Maybe Geolocation.Geolocation     -- ^ The GPS device
                , map           ::Maybe Map.Map                     -- ^ The Map on the page
                , distance      ::Number                  -- ^ The max search distance
              }

-- | Initial state is no logged in user
initialState :: forall i. i -- ^ Initial input
  -> State                  -- ^ The state
initialState _ =  { alert           : Nothing
                    , subscription  : []
                    , geo           : Nothing
                    , map           : Nothing
                    , distance : 300.0}

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

-- The alert banner if there are any problems with this page
nearbyAlert ::forall p i . Maybe String
            -> HH.HTML p i
nearbyAlert (Just t) = HH.div [css "alert alert-danger"] [HH.text $ t]
nearbyAlert Nothing = HH.div [] []

-- |Render the nearby page
render  :: forall m . MonadAff m
        => State                        -- ^ The state to render
        -> H.ComponentHTML Action () m  -- ^ The components HTML
render state = HH.div
               [css "d-flex flex-column ha-nearby"]
               [HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][nearbyAlert state.alert]],
                HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.h2 [][HH.text "Point of interests"]]],
                HH.div [css "row flex-grow-1 ha-nearby-map"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "ha-map"][]]]
                ]

-- |Handles all actions for the login component
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

  -- Create the OpenLayers Map items
  hamap <- H.liftEffect $ createNearbyMap
  gps <- createNearbyGPS hamap

  -- Create the layer
  void $ sequence_ $ addNearbyPOI <$> hamap

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
  state <- H.get
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

  -- Create the view around our world center (should get it from the GPS)
  view <- View.create { projection: Proj.epsg_3857 
                        , center: Proj.fromLonLat [0.0, 0.0] (Just Proj.epsg_3857)
                        , zoom: 18 }

  -- Extend the map with a set of buttons
  ctrl <- Ctrl.defaults {}
  elemAdd <- createMapButton "A" "add-item" "ha-map-add-item"
  elemCenter <- createMapButton "C" "center" "ha-map-center"
  elemRefresh <- createMapButton "R" "refresh" "ha-map-refresh"
  domDocument <- window >>= WHW.document <#> WHHD.toDocument
  elem <- WDD.createElement "div" domDocument
  WDE.setClassName "ha-map-ctrl ol-unselectable ol-control" elem
  void $ WDN.appendChild (WDE.toNode elemAdd) (WDE.toNode elem)
  void $ WDN.appendChild (WDE.toNode elemRefresh) (WDE.toNode elem)
  void $ WDN.appendChild (WDE.toNode elemCenter) (WDE.toNode elem)
  ctrlButtons <- Control.create { element: elem }

  -- Create the map and set up the controls, layers and view
  join <$> (sequence $ Map.create <$> ((\t v -> {
      target: "ha-map"
      , controls: Collection.extend (catMaybes [ctrlButtons]) ctrl
      , layers: [ t ]
      , view: v}) <$> tile <*> view))

  where

    -- Create a button with the given name in the DOM that can be used in the map
    createMapButton :: String                           -- ^The name of the button
                    -> String                           -- ^The id of the map
                    -> String                           -- ^The class name
                    -> Effect WDE.Element   -- ^The map's control
    createMapButton name idt cls = do

      domDocument <- window >>= WHW.document <#> WHHD.toDocument

      txt <- WDD.createTextNode name domDocument
      button <- WDD.createElement "button" domDocument
      WDE.setClassName cls button
      WDE.setId idt button
      void $ WDN.appendChild (WDT.toNode txt) (WDE.toNode button)
      pure button

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
              -> H.HalogenM State Action () o m Unit
addNearbyPOI map = do

  -- Get the weather data from the IoT Hb and our own backend
  entities <- (fromMaybe []) <$> queryEntities "WeatherObserved"
  items <- (fromMaybe []) <$> queryItems {longitude : Nothing
                                          , latitude: Nothing
                                          , distance: Nothing
                                          , limit: Nothing
                                          , text: Nothing }
  
  H.liftEffect do

    -- Create the styles
    olPOIFill <- Fill.create {color: "#32CD32"}
    olIOTFill <- Fill.create {color: "#0080FF"}
    olSYMStroke <- Stroke.create {color: "#000000", width:2}
    olPOIStyle <- Circle.create { radius: 6
      , fill: toNullable olPOIFill
      , stroke: toNullable olSYMStroke }
    olIOTStyle <- Circle.create { radius: 6
      , fill: toNullable olIOTFill
      , stroke: toNullable olSYMStroke }

    -- Create the POI layer
    flist <- sequence $ fromItem <$> items
    vs <- VectorSource.create { features: catMaybes flist }
    vl <- VectorLayer.create { source: toNullable vs}
    sequence_ $ VectorLayer.setStyle <$> (Just (VectorLayer.StyleFunction (poiStyle olPOIStyle) )) <*> vl

    -- Create the IoT Hub Layer
    ilist <- sequence $ fromEntity <$> entities
    ivs <- VectorSource.create { features: catMaybes ilist }
    ivl <- VectorLayer.create { source: toNullable ivs}
    sequence_ $ VectorLayer.setStyle <$> (Just (VectorLayer.StyleFunction (poiStyle olIOTStyle) )) <*> ivl

    -- Add them to the map
    sequence_ $ Map.addLayer <$> vl <*> (Just map)
    sequence_ $ Map.addLayer <$> ivl <*> (Just map)

  where

    -- The style function for the vector layers, returns th estyle based on the feature
    poiStyle::Maybe Circle.Circle->Feature.Feature->Number->Effect (Nullable Style.Style)
    poiStyle poi f r = do
      name <- Feature.get "name" f
      text <- Text.create { text: toNullable name
                            , offsetY: 15
                            , font: "12px Calibri, sans-serif"}
      style <- Style.create { image: toNullable poi,
                              text: toNullable text }
      pure $ toNullable style

    -- Converts from an Entity to a Feature that can b eadded to the IoT Hub Layer
    fromEntity::Entity->Effect (Maybe Feature.Feature)
    fromEntity (Entity e) = do
      -- Not really beautiful to use !! for array access :-(
      point <- Point.create 
        (Proj.fromLonLat [fromMaybe 0.0 $ e.location.value.coordinates!!0
                          , fromMaybe 0.0 $ e.location.value.coordinates!!1]
                          (Just Proj.epsg_3857))
        Nothing      
      Feature.create {name: fromMaybe "?" $ (entityNameTemperature e) <|> (entityNameSnowHeight e)
                      , id: ""
                      , geometry: toNullable point }    

    -- Create the names for the IoTHub entities
    entityNameTemperature en = (flip append "C") <$> (((append "T:") <<< show <<< _.value) <$> en.temperature)
    entityNameSnowHeight  en  = (flip append "mm") <$> (((append "d:") <<< show <<< _.value) <$> en.snowHeight)

    -- Converts from an Item to a Feature that can be added to the Item Layer
    fromItem::Item->Effect (Maybe Feature.Feature)
    fromItem i = do
      point <- Point.create 
        (Proj.fromLonLat [i.longitude, i.latitude] (Just Proj.epsg_3857))
        Nothing
      Feature.create {name: i.name
                      , id: fromMaybe "" i.id
                      , type: 1
                      , geometry: toNullable point }
