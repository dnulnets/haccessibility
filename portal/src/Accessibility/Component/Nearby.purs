-- |
-- | The nearby component
-- |
-- | Written by Tomas Stenlund, Sundsvall,Sweden (c) 2020
-- |
module Accessibility.Component.Nearby (component, Slot(..), Output(..)) where

-- Language imports
import Prelude

-- Data imports
import Data.Array((!!), catMaybes, length, index)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Foldable (sequence_)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Either (Either(..))
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
import OpenLayers.Events.Condition as Condition

import Accessibility.Data.Route (Page(..)) as ADR
import Accessibility.Interface.Endpoint (Problem(..))
import Accessibility.Component.HTML.Utils (css)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Item (class ManageItem, queryItems, Item)
import Accessibility.Interface.Entity (class ManageEntity, queryEntities, Entity(..))

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
                    , distance : 300.0}

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Update
  | EditItem
  | Center
  | AddItem
  | FeatureSelect Select.SelectEvent
  | GPSError
  | GPSPosition Geolocation.Geolocation Feature.Feature
  | GPSAccuracy Geolocation.Geolocation Feature.Feature
  | GPSCenter Geolocation.Geolocation Map.Map

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
               [HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.h2 [][HH.text "Point of interests"]]],
                HH.div [css "row flex-grow-1 ha-nearby-map"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "ha-map"][]]]
                ]

-- |Handles all actions for the login component
handleAction  :: forall r m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Action                               -- ^ The action to handle
              -> H.HalogenM State Action () Output m Unit  -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize Nearby component"

  -- Create the OpenLayers Map items
  hamap <- H.liftEffect $ createNearbyMap
  gps <- createNearbyGPS hamap

  -- Create the layer
  poiLayer <- addNearbyPOI hamap

  -- Add a listener to the add item button on the map
  eadd <- H.liftEffect $ 
    (WHHD.toParentNode <$> (window >>= WHW.document)) >>=
    (WDPN.querySelector (WDPN.QuerySelector "#ha-id-add-item"))  
  sadd <- sequence $ (subscribe AddItem) <$> eadd

  eedit <- H.liftEffect $ 
    (WHHD.toParentNode <$> (window >>= WHW.document)) >>=
    (WDPN.querySelector (WDPN.QuerySelector "#ha-id-edit-item"))  
  sedit <- sequence $ (subscribe EditItem) <$> eedit

  -- Add a listener to the refresh item button on the map
  eupd <- H.liftEffect $ 
    (WHHD.toParentNode <$> (window >>= WHW.document)) >>=
    (WDPN.querySelector (WDPN.QuerySelector "#ha-id-refresh"))  
  supd <- sequence $ (subscribe Update) <$> eupd

  -- Add a listener to the refresh item button on the map
  ecen <- H.liftEffect $ 
    (WHHD.toParentNode <$> (window >>= WHW.document)) >>=
    (WDPN.querySelector (WDPN.QuerySelector "#ha-id-center"))  
  scen <- sequence $ (subscribe Center) <$> ecen

  -- Subscribe for feature selects on the map
  s <- H.liftEffect $ Select.create   { multi: false
                                      , layers: Select.layers.asArray [poiLayer]                                            
                                      , toggleCondition: Condition.never }
  sfeat <- H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Select.onSelect (\e -> do
          HQE.emit emitter (FeatureSelect e)
          pure true) s
        pure (HQE.Finalizer (Select.unSelect key s))
  H.liftEffect $ Map.addInteraction s hamap

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise

  -- Update the stat
  H.modify_ (_ { subscription = (catMaybes [sadd, supd, scen, sedit]) <> [sfeat]
                , map = Just hamap
                , geo = Just gps
                , layer = Just poiLayer
                , select = Just s})

  where

    -- Subscribe to a click event for a button
    subscribe a e = H.subscribe do
      HQE.eventListenerEventSource
        (WEE.EventType "click")
        (WDE.toEventTarget e)
        (const (Just a))

-- | Add an item to the database based on the current position
handleAction AddItem = do
  state <- H.get
  pos <- H.liftEffect do
    log $ "Add and item button clicked"
    join <$> (sequence $ Geolocation.getPosition <$> state.geo)
  sequence_ $ gotoPage <$> (ADR.AddPoint <$> (join (map ((flip index) 1) pos)) <*> (join (map ((flip index) 0) pos)))

-- | Finalize action, clean up the component
handleAction Finalize = do
  H.liftEffect $ log "Finalize Nearby Component"
  state <- H.get
  sequence_ $ H.unsubscribe <$> state.subscription
  H.liftEffect $ do
    sequence_ $ Geolocation.setTracking false <$> state.geo
    sequence_ $ Map.clearTarget <$> state.map
  H.put state { map = Nothing, geo = Nothing, select = Nothing }

-- |Edit the selected item
handleAction EditItem = do
  state <- H.get
  H.liftEffect $ log "Edit a selected item"  
  cf <- H.liftEffect $ sequence $ Select.getFeatures <$> state.select
  sequence_ $ gotoPage <$> (ADR.Point <$> (feature cf) <*> (Just false))
  where
    feature cf = join $ (Feature.get "id") <$> (join $ (Collection.item 0) <$> cf)

-- | Find the items and create a layer and display it
handleAction Update = do
  state <- H.get

  -- Clear the alert
  H.modify_ $ _ {alert = Nothing}
  
  -- Get the POI from our own backend
  ditems <- queryItems {longitude : Nothing
                        , latitude: Nothing
                        , distance: Nothing
                        , limit: Nothing
                        , text: Nothing }

  -- Did we get them
  vs <- case ditems of

    -- Yes we got a response
    Right items -> do
      H.liftEffect do
        flist <- sequence $ fromItem <$> items
        VectorSource.create { features: VectorSource.features.asArray flist }        

    -- No, we are not authenticated, go to login
    Left NotAuthenticated -> do
      H.modify_ $ _ {alert = Just "Authentication failed, please login again!"}
      H.raise AuthenticationError
      H.liftEffect VectorSource.create'

    -- No the backend is gone, the user can retry the update later
    Left Backend -> do
      H.modify_ $ _ {alert = Just "Server is not responding, try again later"}
      H.liftEffect VectorSource.create'

  -- Set the source to the POI-layer
  H.liftEffect $ sequence_ $ (VectorLayer.setSource vs) <$> state.layer

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise

  where

    -- Converts from an Item to a Feature that can be added to the Item Layer
    fromItem::Item->Effect Feature.Feature
    fromItem i = do
      point <- Point.create 
        (Proj.fromLonLat [i.longitude, i.latitude] (Just Proj.epsg_3857))
        Nothing
      Feature.create $ Feature.Properties {name: i.name
                                          , id: fromMaybe "" i.id
                                          , type: 1
                                          , geometry: point }


-- | Find the items
handleAction Center = do
  state <- H.get
  H.liftEffect do
    view <- join <$> (sequence $ Map.getView <$> state.map)
    pos <- join <$> (sequence $ Geolocation.getPosition <$> state.geo)
    sequence_ $ View.setCenter <$> pos <*> view

-- | Feature is selected
handleAction (FeatureSelect e) = H.liftEffect $ do
  log "Feature selected!"

-- | GPS Error - Error in the geolocation device
handleAction GPSError = H.liftEffect $ do
  log "GPS Error!!!"

-- | GPS Position - Position the current location on the map
handleAction (GPSPosition geo feature) = H.liftEffect $ do
  pos <- Geolocation.getPosition geo
  point <- sequence $ Point.create' <$> pos
  sequence_ $ Feature.setGeometry <$> point <*> (Just feature)

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
createNearbyMap::Effect Map.Map
createNearbyMap = do

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
  elemEdit <- createMapButton "E" "ha-id-edit-item" "ha-map-edit-item"
  elemCenter <- createMapButton "C" "ha-id-center" "ha-map-center"
  elemRefresh <- createMapButton "R" "ha-id-refresh" "ha-map-refresh"
  domDocument <- window >>= WHW.document <#> WHHD.toDocument
  elem <- WDD.createElement "div" domDocument
  WDE.setClassName "ha-map-ctrl ol-unselectable ol-control" elem
  void $ WDN.appendChild (WDE.toNode elemAdd) (WDE.toNode elem)
  void $ WDN.appendChild (WDE.toNode elemEdit) (WDE.toNode elem)
  void $ WDN.appendChild (WDE.toNode elemRefresh) (WDE.toNode elem)
  void $ WDN.appendChild (WDE.toNode elemCenter) (WDE.toNode elem)
  ctrlButtons <- Control.create { element: elem }

  -- Create the map and set up the controls, layers and view
  Map.create {
      target: Map.target.asId "ha-map"
      , controls: Map.controls.asCollection $ Collection.extend ([ctrlButtons]) ctrl
      , layers: Map.layers.asArray [ tile ]
      , view: view}

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
-- Create the GPS and add all handlers
--
createNearbyGPS:: forall r o m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Map.Map
              -> H.HalogenM State Action () o m Geolocation.Geolocation
createNearbyGPS map = do

  -- Create the GPS device
  gps <- H.liftEffect $ Geolocation.create {
      trackingOptions: { enableHighAccuracy: true}
      , projection: Proj.epsg_3857
    }

  -- Set up all handlers and features
  setupNearbyGPS gps
  pure gps

  where

    setupNearbyGPSPositionHandler geo feat = do    
      -- Change of Position
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangePosition (\_ -> do
          HQE.emit emitter (GPSPosition geo feat)
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unChangePosition key geo))

    setupNearbyGPSAccuracyHandler geo feat = do
      -- Change of Accuracy
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onChangeAccuracyGeometry (\_ -> do
          HQE.emit emitter (GPSAccuracy geo feat)
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unChangeAccuracyGeometry key geo))

    setupNearbyGPS geo = do
      -- Create the GPS Error handler
      void $ H.subscribe $ HQE.effectEventSource \emitter -> do
        key <- Geolocation.onError (\_ -> do
          HQE.emit emitter GPSError
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unError key geo))

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
      setupNearbyGPSPositionHandler geo $ fst mfeat
      setupNearbyGPSAccuracyHandler geo $ snd mfeat

      -- Turn on the geo location device
      H.liftEffect $ Geolocation.setTracking true geo

      -- Get the current position and position the map
      void $ H.subscribe' $ \_ -> (HQE.effectEventSource \emitter -> do
        key <- Geolocation.onceChangePosition (\_ -> do
          HQE.emit emitter (GPSCenter geo map)
          pure true) geo
        pure (HQE.Finalizer (Geolocation.unChangePosition key geo)))

--
-- Create the layer and add our POI and data  from the IoTHub
--
addNearbyPOI:: forall r m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Map.Map
              -> H.HalogenM State Action () Output m VectorLayer.Vector
addNearbyPOI map = do
  
  -- Get the weather data from the IoT Hb and our own backend
  dentities <- queryEntities "WeatherObserved"

  ivs <- case dentities of
    Left Backend -> do
      H.modify_ $ _ {alert = Just "Server is not responding, try again later"}
      H.liftEffect $ VectorSource.create'
    Left NotAuthenticated -> do
      H.raise AuthenticationError
      H.liftEffect $ VectorSource.create'
    Right items -> do
      ilist <- H.liftEffect $ sequence $ fromEntity <$> items
      H.liftEffect $ VectorSource.create { features: VectorSource.features.asArray $ catMaybes ilist }

  ditems <- queryItems {longitude : Nothing
                        , latitude: Nothing
                        , distance: Nothing
                        , limit: Nothing
                        , text: Nothing }
  
  -- Create the POI source
  vs <- case ditems of
    Left Backend -> do
      H.modify_ $ _ {alert = Just "Server is not responding, try again later"}
      H.liftEffect $ VectorSource.create'
    Left NotAuthenticated -> do
      H.raise AuthenticationError
      H.liftEffect $ VectorSource.create'
    Right items -> do
      flist <- H.liftEffect $ sequence $ fromItem <$> items
      H.liftEffect $ VectorSource.create { features: VectorSource.features.asArray flist }

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
    vl <- VectorLayer.create { source: vs }
    VectorLayer.setStyle (VectorLayer.StyleFunction (poiStyle olPOIStyle)) vl
    Map.addLayer vl map

    -- Create the IoT Hub Layer
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
          text <- Text.create {text: fromMaybe "" name
                              , offsetY: 15
                              , font: "12px Calibri, sans-serif"}
          Style.setText (Just text) style
      pure $ Just style
      where
        name = Feature.get "name" f

    -- Converts from an Entity to a Feature that can b eadded to the IoT Hub Layer
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
    entityNameTemperature en = (flip append "C") <$> (((append "T:") <<< show <<< _.value) <$> en.temperature)
    entityNameSnowHeight  en  = (flip append "mm") <$> (((append "d:") <<< show <<< _.value) <$> en.snowHeight)

    -- Converts from an Item to a Feature that can be added to the Item Layer
    fromItem::Item->Effect Feature.Feature
    fromItem i = do
      point <- Point.create 
        (Proj.fromLonLat [i.longitude, i.latitude] (Just Proj.epsg_3857))
        Nothing
      Feature.create $ Feature.Properties {name: i.name
                                          , id: fromMaybe "" i.id
                                          , type: 1
                                          , geometry: point }
