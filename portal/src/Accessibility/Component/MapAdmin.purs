-- |
-- | The MapAdmin component
-- |
-- | Written by Tomas Stenlund, Sundsvall,Sweden (c) 2020
-- |
module Accessibility.Component.MapAdmin (component, Slot(..), Output(..)) where

-- Language imports
import Prelude

-- Data imports
import Data.Array((!!), catMaybes, length)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe')
import Data.Foldable (sequence_, for_)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Math (pi)

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
import Web.Event.Event as WEE

import Web.HTML (window)
import Web.HTML.Window as WHW
import Web.HTML.HTMLDocument as WHHD

import Web.DOM.Document as WDD
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.DOM.Text as WDT
import Web.DOM.ParentNode as WDPN

-- Openlayers imports
import OpenLayers.Interaction.Select as Select
import OpenLayers.MapBrowserEvent as MapBrowserEvent
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
import OpenLayers.Events.Condition as Condition
import OpenLayers.Coordinate as Coordinate
import OpenLayers.Style.RegularShape as RegularShape

-- Our own imports
import Accessibility.Data.Route (Page(..)) as ADR
import Accessibility.Component.HTML.Utils (css)
import Accessibility.Utils.Result (evaluateResult)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Item (class ManageItem, queryItems, deleteItem, Item)
import Accessibility.Interface.Entity (class ManageEntity, Value, queryEntities, Entity(..))

-- | Slot type for the component
type Slot p = forall q . H.Slot q Output p

-- | State for the component
type State =  { subscription  ::Array H.SubscriptionId  -- ^ The map button subscriptions
                , alert         ::Maybe String          -- ^ Any alert
                , geo           ::Maybe Geolocation.Geolocation     -- ^ The GPS device
                , map           ::Maybe Map.Map                     -- ^ The Map on the page
                , layer         ::Maybe VectorLayer.Vector  -- ^The vector layer for our own poi:s
                , select        ::Maybe Select.Select     -- ^ The select interaction
                , distance      ::Number                  -- ^ The max search distance
                , crosshair     ::Maybe Coordinate.Coordinate -- ^ The coordinate of the crosshair
              }

-- | Initial state is no logged in user
initialState :: forall i. i -- ^ Initial input
  -> State                  -- ^ The state
initialState _ =  { subscription  : []
                    , alert         : Nothing
                    , geo           : Nothing
                    , map           : Nothing
                    , select        : Nothing
                    , layer         : Nothing
                    , crosshair     : Nothing
                    , distance : 1000.0 }

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Update
  | UpdateCursor
  | EditItem
  | DeleteItem
  | Center
  | CenterCursor
  | AddItem
  | AddItemCursor
  | FeatureSelect Select.SelectEvent
  | GPSError
  | GPSPosition Geolocation.Geolocation Feature.Feature
  | GPSAccuracy Geolocation.Geolocation Feature.Feature
  | GPSCenter Geolocation.Geolocation Map.Map VectorLayer.Vector
  | MAPPosition Feature.Feature MapBrowserEvent.MapBrowserEvent

-- | The output from this component
data Output = AuthenticationError
  | Alert (Maybe String)

-- | The component definition
component :: forall r q i m . MonadAff m
          => ManageNavigation m
          => MonadAsk r m
          => ManageEntity m
          => ManageItem m
          => H.Component HH.HTML q i Output m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
      initialize = Just Initialize,
      finalize = Just Finalize
     }
    }

-- |Render the nearby page
render  :: forall m . MonadAff m
        => State                        -- ^ The state to render
        -> H.ComponentHTML Action () m  -- ^ The components HTML
render state = HH.div
               [css "d-flex flex-column ha-nearby"]
               [HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.h2 [][HH.text "POI Administration"]]],
                HH.div [css "row flex-grow-1 ha-nearby-map"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "ha-map"][]]]
                ]

-- |Handles all actions for the login component
handleAction  :: forall r m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Action                                     -- ^ The action to handle
              -> H.HalogenM State Action () Output m Unit   -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize MapAdmin component"

  -- Create the map, layers and handlers
  hamap <- createMap
  poiLayer <- createLayers hamap
  gps <- createGPS hamap poiLayer

  -- Create the handlers
  ba <- createButtonHandlers
  s <- createSelectHandler hamap poiLayer

  -- Set the alert if any
  (Alert <$> H.gets _.alert) >>= H.raise

  -- Update the stat
  H.modify_ (_ { subscription = ba
                , select = Just s
                , map = Just hamap
                , geo = Just gps
                , layer = Just poiLayer})

-- | Add an item to the database based on the current position
handleAction AddItem = do
  state <- H.get
  mpos <- H.liftEffect $ join <$> traverse Geolocation.getPosition state.geo
  for_ mpos \pos -> do
    sequence_ $ gotoPage <$> (ADR.AddPoint  <$> (Coordinate.latitude $ Proj.toLonLat' pos) 
                                            <*> (Coordinate.longitude $ Proj.toLonLat' pos))

-- | Finalize action, clean up the component
handleAction Finalize = do
  state <- H.get
  sequence_ $ H.unsubscribe <$> state.subscription
  H.liftEffect $ do
    sequence_ $ (Geolocation.setTracking false) <$> state.geo
    sequence_ $ Map.clearTarget <$> state.map
  H.put state { map = Nothing, geo = Nothing, select = Nothing }

-- | Add an item to the database based on the current position
handleAction AddItemCursor = do
  state <- H.get
  for_ state.crosshair \pos -> do
    sequence_ $ gotoPage <$> (ADR.AddPoint  <$> (Coordinate.latitude $ Proj.toLonLat' pos) 
                                            <*> (Coordinate.longitude $ Proj.toLonLat' pos))

-- |Edit the selected item
handleAction EditItem = do
  state <- H.get
  H.liftEffect $ log "Edit a selected item"  
  cf <- H.liftEffect $ sequence $ Select.getFeatures <$> state.select
  sequence_ $ gotoPage <$> (ADR.Point <$> (feature cf) <*> (Just false))
  where
    feature cf = join $ (Feature.get "id") <$> (join $ (Collection.item 0) <$> cf)

-- |Delete the selected item
handleAction DeleteItem = do
  state <- H.modify $ _ {alert = Nothing}
  cf <- H.liftEffect $ sequence $ Select.getFeatures <$> state.select
  mr <- sequence $ deleteItem <$> (feature cf)
  void $ sequence $ (evaluateResult AuthenticationError) <$> mr
  src <- join <$> (H.liftEffect $ sequence $ VectorLayer.getSource <$> state.layer)
  H.liftEffect $ sequence_ $ VectorSource.removeFeature <$> (join $ (Collection.item 0) <$> cf) <*> src 

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise

  where
    feature cf = join $ (Feature.get "id") <$> (join $ (Collection.item 0) <$> cf)

-- | Update the POI around the GPS location
handleAction Update = do

  -- Clear the alert
  state <- H.modify $ _ {alert = Nothing}

  -- Get the GPS position
  pos <- H.liftEffect $ join <$> (sequence $ Geolocation.getPosition <$> state.geo)
  
  -- Get the POI from our own backend
  when (isJust pos) do

    ditems <- queryItems {longitude : join $ (Coordinate.longitude <<< Proj.toLonLat') <$> pos
                          , latitude: join $ (Coordinate.latitude <<< Proj.toLonLat') <$> pos
                          , distance: Just state.distance
                          , limit: Nothing
                          , text: Nothing } >>= evaluateResult AuthenticationError  
    vs <- H.liftEffect $ maybe' (\_->VectorSource.create') (\i->do
      flist <- sequence $ fromItem <$> i
      VectorSource.create { features: VectorSource.features.asArray flist }) ditems

    -- Set the source to the POI-layer
    H.liftEffect $ sequence_ $ (VectorLayer.setSource vs) <$> state.layer

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise

-- | Update the POI around the current cursor/crosshair
handleAction UpdateCursor = do

  -- Clear the alert
  state <- H.modify $ _ {alert = Nothing}
  
  -- Get the POI from our own backend
  when (isJust state.crosshair) do

    ditems <- queryItems {longitude : join $ (Coordinate.longitude <<< Proj.toLonLat') <$> state.crosshair
                          , latitude: join $ (Coordinate.latitude <<< Proj.toLonLat') <$> state.crosshair
                          , distance: Just state.distance
                          , limit: Nothing
                          , text: Nothing } >>= evaluateResult AuthenticationError  
    vs <- H.liftEffect $ maybe' (\_->VectorSource.create') (\i->do
      flist <- sequence $ fromItem <$> i
      VectorSource.create { features: VectorSource.features.asArray flist }) ditems

    -- Set the source to the POI-layer
    H.liftEffect $ sequence_ $ (VectorLayer.setSource vs) <$> state.layer

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise

-- | Ceter the map around the GPS position
handleAction Center = do
  state <- H.get
  H.liftEffect do
    pos <- join <$> (sequence $ Geolocation.getPosition <$> state.geo)
    view <- join <$> (sequence $ Map.getView <$> state.map)
    sequence_ $ View.setCenter <$> pos <*> view

-- | Center the map around the Cursor/Crosshair
handleAction CenterCursor = do
  state <- H.get
  H.liftEffect do
    view <- join <$> (sequence $ Map.getView <$> state.map)
    sequence_ $ View.setCenter <$> state.crosshair <*> view

-- | Feature is selected
handleAction (FeatureSelect e) = H.liftEffect $ do
  log "Feature selected!"

-- | GPS Error - Error in the geolocation device
handleAction GPSError = H.liftEffect $ do
  log "GPS Error!!!"

-- | GPS Position - Position the current location on the map
handleAction (GPSPosition geo feature) = do
  pos <- H.liftEffect $ Geolocation.getPosition geo
  H.liftEffect $ do
    point <- sequence $ Point.create' <$> pos
    sequence_ $ Feature.setGeometry <$> point <*> (Just feature)

-- | GPS Accuracy - Position the accuracy polygon on the map
handleAction (GPSAccuracy geo feature) = H.liftEffect $ do
  polygon <- Geolocation.getAccuracyGeometry geo
  Feature.setGeometry polygon feature

-- | GPS Center - Center the map based on geolocation and add all POI:s
handleAction (GPSCenter geo map vl) = do
  pos <- H.liftEffect $ Geolocation.getPosition geo
  H.liftEffect $ do
    mv <- Map.getView map
    sequence_ $ View.setCenter <$> pos <*> mv

  -- Get the POI from our own backend
  state <- H.get
  when (isJust pos) do

    ditems <- queryItems {longitude : join $ (Coordinate.longitude <<< Proj.toLonLat') <$> pos
                          , latitude: join $ (Coordinate.latitude <<< Proj.toLonLat') <$> pos
                          , distance: Just state.distance
                          , limit: Nothing
                          , text: Nothing } >>= evaluateResult AuthenticationError  
    vs <- H.liftEffect $ maybe' (\_->VectorSource.create') (\i->do
      flist <- sequence $ fromItem <$> i
      VectorSource.create { features: VectorSource.features.asArray flist }) ditems

    -- Set the source to the POI-layer
    H.liftEffect $ VectorLayer.setSource vs vl

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise    

-- | Position the cursor/croasshair on the MAP
handleAction (MAPPosition f mbe) = do
  H.modify_ $ _ {crosshair = Just $ MapBrowserEvent.coordinate mbe}
  H.liftEffect $ do
    point <- Point.create' $ MapBrowserEvent.coordinate mbe
    Feature.setGeometry point f

--
-- Creates the map and attaches openstreetmap as a source
--
createMap :: forall o m . MonadAff m
          => H.HalogenM State Action () o m Map.Map
createMap = do

  hamap <- H.liftEffect $ do

    -- Use OpenStreetMap as a source
    osm <- OSM.create'
    tile <- Tile.create {source: osm}

    -- Create the view around our world center (should get it from the GPS)
    view <- View.create { projection: Proj.epsg_3857 
                        , center: Proj.fromLonLat [0.0, 0.0] (Just Proj.epsg_3857)
                        , zoom: 18.0 }

    -- Extend the map with a set of buttons
    ctrl <- Ctrl.defaults'
    elemAdd <- createMapButton "A" "ha-id-add-item" "ha-map-add-item"
    elemSAdd <- createMapButton "a" "ha-id-sadd-item" "ha-map-sadd-item"
    elemEdit <- createMapButton "E" "ha-id-edit-item" "ha-map-edit-item"
    elemDelete <- createMapButton "D" "ha-id-delete-item" "ha-map-delete-item"
    elemCenter <- createMapButton "C" "ha-id-center" "ha-map-center"
    elemSCenter <- createMapButton "c" "ha-id-scenter" "ha-map-scenter"
    elemRefresh <- createMapButton "R" "ha-id-refresh" "ha-map-refresh"
    elemSRefresh <- createMapButton "r" "ha-id-srefresh" "ha-map-srefresh"

    domDocument <- window >>= WHW.document <#> WHHD.toDocument
    elem <- WDD.createElement "div" domDocument
    WDE.setClassName "ha-map-ctrl ol-unselectable ol-control" elem

    elem1 <- WDD.createElement "div" domDocument
    WDE.setClassName "tomas" elem1
    void $ WDN.appendChild (WDE.toNode elemAdd) (WDE.toNode elem1)
    void $ WDN.appendChild (WDE.toNode elemSAdd) (WDE.toNode elem1)
    void $ WDN.appendChild (WDE.toNode elem1) (WDE.toNode elem)

    elem2 <- WDD.createElement "div" domDocument
    WDE.setClassName "tomas" elem2
    void $ WDN.appendChild (WDE.toNode elemEdit) (WDE.toNode elem2)
    void $ WDN.appendChild (WDE.toNode elemDelete) (WDE.toNode elem2)
    void $ WDN.appendChild (WDE.toNode elem2) (WDE.toNode elem)

    elem3 <- WDD.createElement "div" domDocument
    WDE.setClassName "tomas" elem3
    void $ WDN.appendChild (WDE.toNode elemRefresh) (WDE.toNode elem3)
    void $ WDN.appendChild (WDE.toNode elemSRefresh) (WDE.toNode elem3)
    void $ WDN.appendChild (WDE.toNode elem3) (WDE.toNode elem)

    elem4 <- WDD.createElement "div" domDocument
    WDE.setClassName "tomas" elem4
    void $ WDN.appendChild (WDE.toNode elemCenter) (WDE.toNode elem4)
    void $ WDN.appendChild (WDE.toNode elemSCenter) (WDE.toNode elem4)
    void $ WDN.appendChild (WDE.toNode elem4) (WDE.toNode elem)

    ctrlButtons <- Control.create { element: elem }

    -- Create the map and set up the controls, layers and view
    Map.create {
        target: Map.target.asId "ha-map"
        , controls: Map.controls.asCollection $ Collection.extend ([ctrlButtons]) ctrl
        , layers: Map.layers.asArray [ tile ]
        , view: view}

  -- Return with the map
  pure hamap

  where

    -- Create a button with the given name in the DOM that can be used in the map
    createMapButton :: String                           -- ^The name of the button
                    -> String                           -- ^The id of the map
                    -> String                           -- ^The class name
                    -> Effect WDE.Element   -- ^The map's control
    createMapButton name idt cls = do

      -- Get hold of the DOM
      domDocument <- window >>= WHW.document <#> WHHD.toDocument

      -- Create the textnode and the button
      txt <- WDD.createTextNode name domDocument
      button <- WDD.createElement "button" domDocument
      WDE.setClassName cls button
      WDE.setId idt button
      void $ WDN.appendChild (WDT.toNode txt) (WDE.toNode button)
      pure button

--
-- Creates the select interaction
--
createButtonHandlers:: forall o m . MonadAff m
                    => H.HalogenM State Action () o m (Array H.SubscriptionId)
createButtonHandlers = do

  -- Add a listener to every button on the map
  sadd <- addMapButtonHandler AddItem "#ha-id-add-item"
  ssadd <- addMapButtonHandler AddItemCursor "#ha-id-sadd-item"
  sedit <- addMapButtonHandler EditItem "#ha-id-edit-item"
  sdelete <- addMapButtonHandler DeleteItem "#ha-id-delete-item"
  supd <-  addMapButtonHandler Update "#ha-id-refresh"
  ssupd <-  addMapButtonHandler UpdateCursor "#ha-id-srefresh"
  scen <-  addMapButtonHandler Center "#ha-id-center"
  sscen <-  addMapButtonHandler CenterCursor "#ha-id-scenter"
  pure $ catMaybes [sadd, sedit, supd, scen, ssadd, ssupd, sscen, sdelete]

  where

    -- Add a handler to a map button
    addMapButtonHandler a id = do 
      elem <- H.liftEffect $ 
        (WHHD.toParentNode <$> (window >>= WHW.document)) >>=
        (WDPN.querySelector (WDPN.QuerySelector id))  
      sequence $ (subscribeOnClick a) <$> elem

    -- Subscribe to a click event for a button
    subscribeOnClick a e = H.subscribe do
      HQE.eventListenerEventSource
        (WEE.EventType "click")
        (WDE.toEventTarget e)
        (const (Just a))

--
-- Creates the select interaction
--
createSelectHandler :: forall o m . MonadAff m
                    => Map.Map
                    -> VectorLayer.Vector
                    -> H.HalogenM State Action () o m Select.Select
createSelectHandler hamap poiLayer = do
  -- Subscribe for feature selects on the map
  fs <- H.liftEffect $ Select.create   { multi: false
                                        , layers: Select.layers.asArray [poiLayer]                                            
                                        , toggleCondition: Condition.never }
  sfeat <- H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Select.onSelect (\e -> do
          HQE.emit emitter (FeatureSelect e)
          pure true) fs
        pure (HQE.Finalizer (Select.unSelect key fs))
  H.liftEffect $ Map.addInteraction fs hamap
  pure fs

--
-- Create the GPS and add all handlers
--
createGPS :: forall o m . MonadAff m
          => Map.Map
          -> VectorLayer.Vector
          -> H.HalogenM State Action () o m Geolocation.Geolocation
createGPS map vl = do

  geo <- H.liftEffect $ Geolocation.create { trackingOptions: { enableHighAccuracy: true}
                                            , projection: Proj.epsg_3857 }

  -- Create the GPS Position Feature, a dot with a circle
  mfeat <- H.liftEffect $ do
      pfill <- Fill.create { color: Fill.color.asString "#3399CC" }
      pstroke <- Stroke.create { color: Stroke.color.asString "#fff", width: 2}
      pcircle <- Circle.create {
      radius: 6.0
      , fill: pfill
      , stroke: pstroke
      }
      pstyle <- Style.create {image: pcircle}
      pfeat <- Feature.create'
      pafeat <- Feature.create'
      Feature.setStyle (Just pstyle) pfeat
      psvector <- VectorSource.create {features: VectorSource.features.asArray [pfeat, pafeat]}
      plvector <- VectorLayer.create { source: psvector }
      Map.addLayer plvector map
      pure $ Tuple pfeat pafeat

  -- Event handlers for the GPS Position
  setupGPSErrorHandler geo
  setupGPSPositionHandler geo $ fst mfeat
  setupGPSAccuracyHandler geo $ snd mfeat

  -- Turn on the geo location device
  H.liftEffect $ Geolocation.setTracking true geo

  -- Get the current position and position the map, one time
  void $ H.subscribe' $ \_ -> (HQE.effectEventSource \emitter -> do
    key <- Geolocation.onceChangePosition (\_ -> do
      HQE.emit emitter (GPSCenter geo map vl)
      pure true) geo
    pure (HQE.Finalizer (Geolocation.unChangePosition key geo)))

  pure geo

  where

    setupGPSPositionHandler geo feat = do    
      -- Change of Position
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangePosition (\_ -> do
          HQE.emit emitter (GPSPosition geo feat)
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unChangePosition key geo))

    setupGPSAccuracyHandler geo feat = do
      -- Change of Accuracy
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangeAccuracyGeometry (\_ -> do
          HQE.emit emitter (GPSAccuracy geo feat)
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unChangeAccuracyGeometry key geo))

    setupGPSErrorHandler geo = do
    -- Create the GPS Error handler
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onError (\_ -> do
          HQE.emit emitter GPSError
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unError key geo))

--
-- Create the layer and add our POI and data  from the IoTHub
--
createLayers:: forall m . MonadAff m
            => ManageEntity m
            => Map.Map
            -> H.HalogenM State Action () Output m VectorLayer.Vector
createLayers map = do
  
  -- Create Crosshair/Cursor Layer
  fcursor <- H.liftEffect do

    -- Create the styles
    olFill <- Fill.create {color: Fill.color.asString "#FF0000"}
    olStroke <- Stroke.create {color: Stroke.color.asString "#000000", width:2}
    olStyle <- RegularShape.create { fill: olFill
      , stroke: olStroke
      , points: 4
      , radius: 10
      , radius2: 0
      , angle: pi/4.0}

    pstyle <- Style.create {image: olStyle}
    pfeat <- Feature.create'
    Feature.setStyle (Just pstyle) pfeat
    psvector <- VectorSource.create {features: VectorSource.features.asArray [pfeat]}
    plvector <- VectorLayer.create { source: psvector }
    Map.addLayer plvector map
    pure pfeat
    
  -- Get a MapBrowser Event for singleclick
  void $ H.subscribe $ HQE.effectEventSource \emitter -> do
    key <- Map.on "singleclick" (\e -> do
      HQE.emit emitter (MAPPosition fcursor e)
      pure true) map
    pure (HQE.Finalizer (Map.un "singleclick" key map))

  -- Get the weather data from the IoT Hub
  dentities <- queryEntities "WeatherObserved" >>= evaluateResult AuthenticationError
  ivs <- H.liftEffect $ maybe' (\_->VectorSource.create') (\i->do
    ilist <- sequence $ fromEntity <$> i
    VectorSource.create { features: VectorSource.features.asArray $ catMaybes ilist }) dentities

  -- We need the distance
  state <- H.get

  -- POI and IoTHub layer
  H.liftEffect do

    -- Create the styles
    olPOIFill <- Fill.create {color: Fill.color.asString "#32CD32"}
    olIOTFill <- Fill.create {color: Fill.color.asString "#0080FF"}
    olSYMStroke <- Stroke.create {color: Stroke.color.asString "#000000", width:2}
    olPOIStyle <- Circle.create { radius: 6.0
      , fill: olPOIFill
      , stroke: olSYMStroke }
    olIOTStyle <- Circle.create { radius: 6.0
      , fill: olIOTFill
      , stroke: olSYMStroke }

    -- Create the POI Layer
    vl <- VectorLayer.create'
    VectorLayer.setStyle (VectorLayer.StyleFunction (poiStyle olPOIStyle)) vl
    Map.addLayer vl map

    -- Create the IoT Hub Layer and add the IoTHub source
    ivl <- VectorLayer.create { source: ivs }
    VectorLayer.setStyle (VectorLayer.StyleFunction (poiStyle olIOTStyle)) ivl
    Map.addLayer ivl map

    -- Return with the POI layer
    pure vl

  where

    -- The style function for the vector layers, returns the style based on the feature
    poiStyle::Circle.CircleStyle->Feature.Feature->Number->Effect (Maybe Style.Style)
    poiStyle poi f r = do
      style <- Style.create { image: poi }
      when (isJust name) do
          text <- Text.create {text: fromMaybe "<unknown>" name
                              , offsetY: 15
                              , font: "12px Calibri, sans-serif"}
          Style.setText (Just text) style
      pure $ Just style
      where
        name = Feature.get "name" f

-- Converts from an Entity to a Feature that can b added to the IoT Hub Layer
fromEntity::Entity->Effect (Maybe Feature.Feature)
fromEntity (Entity e) = do
  case length e.location.value.coordinates of
    2 -> do
      point <- Point.create' $ Proj.fromLonLat [fromMaybe 0.0 $ e.location.value.coordinates!!0
                                                , fromMaybe 0.0 $ e.location.value.coordinates!!1]
                                                (Just Proj.epsg_3857)
      feature <- Feature.create $ Feature.Properties { name: fromMaybe "?" $ (entityNameTemperature e) <|> (entityNameSnowHeight e)
                                                      , geometry: point }
      pure $ Just feature
    _ -> pure Nothing

-- Create the names for the IoTHub entities
entityNameTemperature::forall r . {temperature::Maybe Value|r}->Maybe String
entityNameTemperature en = (flip append "C") <$> (((append "T:") <<< show <<< _.value) <$> en.temperature)

entityNameSnowHeight::forall r . {snowHeight::Maybe Value|r}->Maybe String
entityNameSnowHeight  en = (flip append "mm") <$> (((append "d:") <<< show <<< _.value) <$> en.snowHeight)

-- Converts from an Item to a Feature that can be added to the Item Layer
fromItem::Item->Effect Feature.Feature
fromItem i = do
  point <- Point.create 
    (Proj.fromLonLat [i.longitude, i.latitude] (Just Proj.epsg_3857))
    Nothing
  Feature.create $ Feature.Properties {name: i.name
                                      , id: fromMaybe "<unknown>" i.id
                                      , type: 1
                                      , geometry: point }
