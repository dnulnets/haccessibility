-- |
-- | The MapUser component
-- |
-- | Written by Tomas Stenlund, Sundsvall,Sweden (c) 2020
-- |
module Accessibility.Component.MapUser (component, Slot(..), Output(..), Input(..)) where

-- Language imports

import Prelude

import Accessibility.Component.HTML.Utils (css)
import Accessibility.Interface.Entity (class ManageEntity, Value, Entity(..))
import Accessibility.Interface.Item (class ManageItem, Item, queryItemsAndValues)
import Accessibility.Interface.Navigate (class ManageNavigation)
import Accessibility.Interface.User (class ManageUser, UserProperty, queryUserProperties)
import Accessibility.Utils.Result (evaluateResult)
import Accessibility.Utils.DOM (setInnerHTML)

import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (class MonadAsk)

import Data.Array ((!!), catMaybes, length, head, foldr)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe')
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)

import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as HQE

import Math (pi)

import OpenLayers.Collection as Collection
import OpenLayers.Control as Ctrl
import OpenLayers.Control.Control as Control
import OpenLayers.Coordinate as Coordinate
import OpenLayers.Events.Condition as Condition
import OpenLayers.Feature as Feature
import OpenLayers.Geolocation as Geolocation
import OpenLayers.Geom.Point as Point
import OpenLayers.Interaction.Select as Select
import OpenLayers.Layer.Tile as Tile
import OpenLayers.Layer.Vector as VectorLayer
import OpenLayers.Map as Map
import OpenLayers.Proj as Proj
import OpenLayers.Render.Event as Event
import OpenLayers.Size as Size
import OpenLayers.Source.OSM as OSM
import OpenLayers.Source.Vector as VectorSource
import OpenLayers.Style.Circle as Circle
import OpenLayers.Style.Fill as Fill
import OpenLayers.Style.RegularShape as RegularShape
import OpenLayers.Style.Stroke as Stroke
import OpenLayers.Style.Style as Style
import OpenLayers.Style.Text as Text
import OpenLayers.View as View
import OpenLayers.Overlay as Overlay
import OpenLayers.MapBrowserEvent as MapBrowserEvent
import Web.DOM.Document as WDD
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.DOM.ParentNode as WDPN
import Web.DOM.Text as WDT
import Web.Event.Event as WEE
import Web.HTML (window)
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.Window as WHW

-- | Slot type for the component
type Slot p = forall q . H.Slot q Output p

-- | State for the component
type State =  { subscription  ::Array H.SubscriptionId  --  The map button subscriptions
                , alert         ::Maybe String          --  Any alert
                , geo           ::Maybe Geolocation.Geolocation --  The GPS device
                , map           ::Maybe Map.Map             -- | The Map on the page
                , layer         ::Maybe VectorLayer.Vector  -- The vector layer for our own poi:s
                , overlay       ::Maybe Overlay.Overlay     -- The overlay for our map
                , content       ::Maybe WDE.Element     -- Content of the overlay popup
                , select        ::Maybe Select.Select     --  The select interaction
                , distance      ::Number                  --  The max search distance
                , crosshair     ::Maybe Coordinate.Coordinate --  The coordinate of the crosshair
                , initial       ::Input                 -- Map location when started
                , userProperties :: Array (UserProperty)  -- List of all user properties
              }

-- Initial map location
type Input = {coordinate::Maybe Coordinate.Coordinate, zoom::Maybe Number}

-- | Initial state of the component is more or less "Nothing"
initialState :: Input --  Initial input
  -> State            --  The state
initialState ml =  { subscription  : []
                    , alert         : Nothing
                    , geo           : Nothing
                    , map           : Nothing
                    , select        : Nothing
                    , layer         : Nothing
                    , overlay       : Nothing
                    , content       : Nothing
                    , crosshair     : Nothing
                    , initial       : ml
                    , userProperties : []
                    , distance : 3000.0 }

-- | Internal form actions
data Action = Initialize
  | Finalize
  | Update
  | Center
  | FeatureSelect Select.SelectEvent
  | ClosePopup
  | GPSError
  | GPSPosition Geolocation.Geolocation Feature.Feature
  | GPSAccuracy Geolocation.Geolocation Feature.Feature
  | GPSCenter Geolocation.Geolocation Map.Map VectorLayer.Vector
  | MAPRenderComplete Event.RenderEvent

-- | The output from this component
data Output = AuthenticationError
  | Alert (Maybe String)
  | MapPosition (Maybe Coordinate.Coordinate) (Maybe Number)

-- | The component definition
component :: forall r q m . MonadAff m
          => ManageNavigation m
          => MonadAsk r m
          => ManageEntity m
          => ManageItem m
          => ManageUser m
          => H.Component HH.HTML q Input Output m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
      initialize = Just Initialize,
      finalize = Just Finalize
     }
    }

-- |Render the MapNearby page
render  :: forall m . MonadAff m
        => State                        --  The state to render
        -> H.ComponentHTML Action () m  --  The components HTML
render state = HH.div
               [css "d-flex flex-column ha-nearby"]
               [HH.div [css "row"] [HH.div[css "col-xs-12 col-md-12"][HH.h2 [][HH.text "Points Of Interest"]]],
                HH.div [css "row flex-grow-1 ha-nearby-map"] [HH.div[css "col-xs-12 col-md-12"][HH.div [HP.id_ "ha-map"][]]]
                ]

-- |Handles all actions for the login component
handleAction  :: forall r m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => ManageUser m
              => Action                                     --  The action to handle
              -> H.HalogenM State Action () Output m Unit   --  The handled action

-- | Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize MapUser component"

  -- Get the user properties, used to evaluate all POI:s
  up <- (queryUserProperties >>= (evaluateResult AuthenticationError))

  -- Create the map, overlay, layers and handlers
  hamap <- createMap
  poiLayer <- createLayers hamap
  Tuple poiOverlay content <- createOverlay hamap
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
                , layer = Just poiLayer
                , overlay = Just poiOverlay
                , content = Just content
                , userProperties = fromMaybe [] up})

-- | Finalize action, clean up the component
handleAction Finalize = do
  state <- H.get
  
  sequence_ $ H.unsubscribe <$> state.subscription
  H.liftEffect $ do
    sequence_ $ (Geolocation.setTracking false) <$> state.geo
    sequence_ $ Map.clearTarget <$> state.map
  H.put state { map = Nothing, geo = Nothing, select = Nothing }

-- | Update the POI around the GPS location
handleAction Update = do

  -- Clear the alert
  state <- H.modify $ _ {alert = Nothing}

  -- Get the MAP center position
  pos <- H.liftEffect do
    view <- join <$> (sequence $ Map.getView <$> state.map)
    join <$> (sequence $ View.getCenter <$> view)

  -- Get the size of the view in meters
  distance <- H.liftEffect do
    view <- join <$> (sequence $ Map.getView <$> state.map)  
    resolution <- join <$> (sequence $ View.getResolution <$> view)
    size <- join <$> (sequence $ Map.getSize <$> state.map)
    log $ "Resolution " <> (show resolution)
    log $ "Size " <> (show size)
    pure $ div  <$> (max <$> (mul <$> resolution <*> (join (Size.width <$> size)))
                        <*> (mul <$> resolution <*> (join (Size.height <$> size))))
                <*> Just 2.0

  H.liftEffect $ log $ "Radius for search is " <> (show distance)
  H.liftEffect $ log $ "Position for search is " <> (show pos)

  -- Get the POI from our own backend
  when (isJust pos) do

    ditems <- queryItemsAndValues {longitude : join $ (Coordinate.longitude <<< Proj.toLonLat') <$> pos
                          , latitude: join $ (Coordinate.latitude <<< Proj.toLonLat') <$> pos
                          , distance: Just $ fromMaybe state.distance distance
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

-- | Feature is selected
handleAction (FeatureSelect e) = do
  state <- H.get
  H.liftEffect $ log "Feature selected!"
  features <- H.liftEffect $ Select.getSelected e
  case head features of
    Nothing -> do
      H.liftEffect $ log "No selected POI"
    Just f -> do
      H.liftEffect $ log $ (show (fromMaybe ["<Nothing>"] (Feature.get "ha_lpos" f)))
      H.liftEffect $ log $ (show (fromMaybe ["<Nothing>"] (Feature.get "ha_lneg" f)))
      H.liftEffect $ log $ (show (fromMaybe ["<Nothing>"] (Feature.get "ha_lunk" f)))
      case value f of
        Nothing ->
          -- H.liftEffect $ sequence_ $ WDN.setTextContent "No information" <$> WDE.toNode <$> state.content
          H.liftEffect $ sequence_ $ flip setInnerHTML "<p>No information</p>" <$> state.content
        Just v ->
          -- H.liftEffect $ sequence_ $ WDN.setTextContent (v.positive <> "\n" <> v.negative <> "<br>" <> v.unknown) <$> WDE.toNode <$> state.content
          H.liftEffect $ sequence_ $ flip setInnerHTML (v.score <> "<br>" <> v.lpos <> 
            "<br>" <> v.lunk <> "<br>" <> v.lneg) <$> state.content
      coord <- H.liftEffect $ MapBrowserEvent.coordinate <$> Select.getMapBrowserEvent e
      H.liftEffect $ sequence_ $ Overlay.setPosition (Just coord) <$> state.overlay

  where

    value::Feature.Feature -> Maybe {score::String, lpos::String, lneg::String, lunk::String}
    value f = value1 <$> (Feature.get "ha_pos" f) <*> (Feature.get "ha_neg" f) <*> (Feature.get "ha_unk" f) <*> 
                         (Feature.get "ha_lpos" f) <*> (Feature.get "ha_lneg" f) <*> (Feature.get "ha_lunk" f)

      where
        value1::Int->Int->Int->Array String->Array String->Array String->{score::String, lpos::String, lneg::String, lunk::String}
        value1 p n u lp ln lu = {
            score: "<b>Score (+/?/-):" <> (show $ p*100/(p+n+u)) <> "/" <> (show $ u*100/(p+n+u)) <> "/" <> (show $ n*100/(p+n+u)) <> "</b>"
            , lpos: foldr (\a la->la <> "<br>" <> a ) "<b>Present:</b>" lp
            , lunk: foldr (\a la->la <> "<br>" <> a ) "<b>Unknown:</b>" lu
            , lneg: foldr (\a la->la <> "<br>" <> a ) "<b>Missing:</b>" ln
          }

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
  state <- H.get

  pos <- H.liftEffect $ Geolocation.getPosition geo
  H.liftEffect $ do
    mv <- Map.getView map
    sequence_ $ View.setCenter <$> pos <*> mv

  -- Create the POI source
  createPOI map vl pos

  -- Set the alert
  (Alert <$> H.gets _.alert) >>= H.raise    

-- |Send the maps center and zoom out from the component
handleAction (MAPRenderComplete e) = do
  state <- H.get
  event <- H.liftEffect $ do
    view <- join <$> (sequence $ Map.getView <$> state.map)
    c <- join <$> (sequence $ View.getCenter <$> view)
    z <- join <$> (sequence $ View.getZoom <$> view)
    pure $ MapPosition c z
  H.raise event

-- | Position the cursor/croasshair on the MAP
handleAction ClosePopup = do
  -- H.liftEffect $ WEE.preventDefault $ toEvent event
  state <- H.get
  _ <- H.liftEffect $ sequence $ Overlay.setPosition Nothing <$> state.overlay
  H.liftEffect $ log $ "Closed popup!"

-- Creates the Point of interest for the map and layer
createPOI :: forall m . MonadAff m
          => ManageItem m
          => Map.Map
          -> VectorLayer.Vector
          -> Maybe Coordinate.Coordinate
          -> H.HalogenM State Action () Output m Unit
createPOI map vl pos = do

  -- Get the state of the component
  state <- H.get

  -- Get the size of the view in meters
  distance <- H.liftEffect do
    view <- Map.getView map
    resolution <- join <$> (sequence $ View.getResolution <$> view)
    size <- Map.getSize map
    log $ "Resolution " <> (show resolution)
    log $ "Size " <> (show size)
    pure $ div  <$> (max <$> (mul <$> resolution <*> (join (Size.width <$> size)))
                        <*> (mul <$> resolution <*> (join (Size.height <$> size))))
                <*> Just 2.0

  H.liftEffect $ log $ "Distance=" <> (show distance)
  H.liftEffect $ log $ "Position=" <> (show pos)

  -- Get the POI from our own backend
  when (isJust pos) do

    ditems <- queryItemsAndValues {longitude : join $ (Coordinate.longitude <<< Proj.toLonLat') <$> pos
                          , latitude: join $ (Coordinate.latitude <<< Proj.toLonLat') <$> pos
                          , distance: Just $ fromMaybe state.distance distance
                          , limit: Nothing
                          , text: Nothing } >>= evaluateResult AuthenticationError  
    vs <- H.liftEffect $ maybe' (\_->VectorSource.create') (\i->do
      flist <- sequence $ fromItem <$> i
      VectorSource.create { features: VectorSource.features.asArray flist }) ditems

    -- Set the source to the POI-layer
    H.liftEffect $ VectorLayer.setSource vs vl
  
--
-- Creates the map and attaches openstreetmap as a source
--
createMap :: forall o m . MonadAff m
          => H.HalogenM State Action () o m Map.Map
createMap = do

  state <- H.get
  hamap <- H.liftEffect $ do

    -- Use OpenStreetMap as a source
    osm <- OSM.create'
    tile <- Tile.create {source: osm}

    -- Create the view around our world center (should get it from the GPS)
    log $ "Got initial state as " <> (show state.initial)
    view <- View.create { projection: Proj.epsg_3857 
                        , center: fromMaybe (Proj.fromLonLat [0.0, 0.0] (Just Proj.epsg_3857)) state.initial.coordinate
                        , zoom: fromMaybe 18.0 state.initial.zoom}

    -- Extend the map with a set of buttons
    ctrl <- Ctrl.defaults'
    elemCenter <- createMapButton "C" "ha-id-center" "ha-map-center"
    elemRefresh <- createMapButton "R" "ha-id-refresh" "ha-map-refresh"

    domDocument <- window >>= WHW.document <#> WHHD.toDocument
    elem <- WDD.createElement "div" domDocument
    WDE.setClassName "ha-map-ctrl ol-unselectable ol-control" elem

    elem1 <- WDD.createElement "div" domDocument
    void $ WDN.appendChild (WDE.toNode elemRefresh) (WDE.toNode elem1)
    void $ WDN.appendChild (WDE.toNode elem1) (WDE.toNode elem)

    elem2 <- WDD.createElement "div" domDocument
    void $ WDN.appendChild (WDE.toNode elemCenter) (WDE.toNode elem2)
    void $ WDN.appendChild (WDE.toNode elem2) (WDE.toNode elem)

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
    createMapButton :: String                           -- The name of the button
                    -> String                           -- The id of the map
                    -> String                           -- The class name
                    -> Effect WDE.Element   -- The map's control
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
  supd <-  addMapButtonHandler Update "#ha-id-refresh"
  scen <-  addMapButtonHandler Center "#ha-id-center"
  pure $ catMaybes [supd, scen]

  where

    -- Add a handler to a map button
    addMapButtonHandler a id = do 
      elem <- H.liftEffect $ 
        (WHHD.toParentNode <$> (window >>= WHW.document)) >>=
        (WDPN.querySelector (WDPN.QuerySelector id))  
      sequence $ (subscribeOnClick a) <$> elem

-- Subscribe to a click event for a button
subscribeOnClick::forall o m . MonadAff m
  => Action->WDE.Element->H.HalogenM State Action () o m H.SubscriptionId
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
createGPS :: forall m . MonadAff m
          => ManageItem m
          => Map.Map
          -> VectorLayer.Vector
          -> H.HalogenM State Action () Output m Geolocation.Geolocation
createGPS map vl = do

  state <- H.get
  geo <- H.liftEffect $ Geolocation.create { trackingOptions: { enableHighAccuracy: true}
                                            , projection: Proj.epsg_3857 }

  -- Create the GPS Position Feature, a dot with a circle
  mfeat <- H.liftEffect $ do
      pfill <- Fill.create { color: Fill.color.asString "#3399CC" }
      pstroke <- Stroke.create { color: Stroke.color.asString "#fff", width: 2}
      pcircle <- Circle.create { radius: 6.0
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

  -- Get the current position and position the map from the GPS, but only once
  when (isNothing state.initial.coordinate) do
    void $ H.subscribe' $ \_ -> (HQE.effectEventSource \emitter -> do
      key <- Geolocation.onceChangePosition (\_ -> do
        HQE.emit emitter (GPSCenter geo map vl)
        pure true) geo
      pure (HQE.Finalizer (Geolocation.unChangePosition key geo)))

  -- Get the current position from the input to the component
  when (isJust state.initial.coordinate) do
    createPOI map vl state.initial.coordinate

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
-- Create the overlay
--
createOverlay:: forall m . MonadAff m
            => ManageEntity m
            => Map.Map
            -> H.HalogenM State Action () Output m (Tuple Overlay.Overlay WDE.Element)
createOverlay map = do

  popup <- H.liftEffect do

    domDocument <- window >>= WHW.document <#> WHHD.toDocument
    popup <- WDD.createElement "div" domDocument
    WDE.setClassName "ol-popup" popup
    WDE.setId "popup" popup

    closer <- WDD.createElement "a" domDocument
    WDE.setClassName "ol-popup-closer" closer
    WDE.setId "popup-closer" closer
    void $ WDN.appendChild (WDE.toNode closer) (WDE.toNode popup)
    
    content <- WDD.createElement "div" domDocument
    WDE.setId "popup-content" content
    --txt <- WDD.createTextNode "Information" domDocument
    --void $ WDN.appendChild (WDT.toNode txt) (WDE.toNode content)
    void $ WDN.appendChild (WDE.toNode content) (WDE.toNode popup)

    pure $ { popup: popup, closer: closer, content:content}

  _ <- subscribeOnClick ClosePopup popup.closer

  olOverlay <- H.liftEffect $ Overlay.create { element: popup.popup
                                              , autoPan: Overlay.autoPan.asBoolean true
                                              , autoPanAnimation: {duration:250}}

  H.liftEffect $ Overlay.setPosition Nothing olOverlay  
  H.liftEffect $ Map.addOverlay olOverlay map
  pure $ Tuple olOverlay popup.content

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
    
  -- Get a RenderComplete Event when the rendering is complete
  void $ H.subscribe $ HQE.effectEventSource \emitter -> do
    key <- Map.onRenderComplete (\e -> do
      HQE.emit emitter (MAPRenderComplete e)
      pure true) map
    pure (HQE.Finalizer (Map.unRenderComplete key map))

  -- Get the weather data from the IoT Hub
  --dentities <- queryEntities "WeatherObserved" >>= evaluateResult AuthenticationError
  --ivs <- H.liftEffect $ maybe' (\_->VectorSource.create') (\i->do
  --  ilist <- sequence $ fromEntity <$> i
  --  VectorSource.create { features: VectorSource.features.asArray $ catMaybes ilist }) dentities
  ivs <- H.liftEffect $ VectorSource.create { features: VectorSource.features.asArray [] }

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
                                      , ha_pos: fromMaybe 0 i.positive
                                      , ha_neg: fromMaybe 0 i.negative
                                      , ha_unk: fromMaybe 0 i.unknown
                                      , ha_lpos: fromMaybe [] i.positiveAttributes
                                      , ha_lneg: fromMaybe [] i.negativeAttributes
                                      , ha_lunk: fromMaybe [] i.unknownAttributes
                                      , type: 1
                                      , geometry: point }
