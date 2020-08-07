-- |
-- | The point component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessibility.Component.Point (component, Operation(..), Slot(..), Output(..)) where

-- Language imports
import Prelude

-- Data imports
import Data.Either (Either(..))
import Data.Array (catMaybes, deleteBy, length)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.DateTime.ISO (ISO(..))
import Data.UUID (genUUID, toString)
import Data.Tuple (Tuple(..))
import Data.Traversable (sequence)
import Data.Map as Map
import Data.List as List

-- Monad imports
import Control.Monad.Reader.Trans (class MonadAsk)

-- Effect imports
import Effect.Now (nowDateTime)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- Halogen imports
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

-- Web imports
import Web.Event.Event (Event)
import Web.Event.Event as Event

-- Openlayers
import OpenLayers.Coordinate as Coordinate

-- Our own imports
import Accessibility.Utils.Result (evaluateResult)
import Accessibility.Component.HTML.Utils (css, prop, enableTooltips)
import Accessibility.Interface.Entity (class ManageEntity)
import Accessibility.Interface.Item (
  class ManageItem
  , AttributeType(..)
  , AttributeValue
  , AttributeChange
  , Item
  , ItemApproval(..)
  , ItemSource(..)
  , ItemModifier(..)
  , addItem
  , updateItem
  , updateItemAttributes
  , queryItem
  , queryAttributes
  , queryItemAttributes)
import Accessibility.Interface.Navigate (class ManageNavigation)

-- | Slot type for the Point component
type Slot p = forall q. H.Slot q Output p

-- | Output from the Point form
data Output = Submitted
  | Alert (Maybe String)
  | AuthenticationError

-- | Internal form actions
data Action = Initialize  -- ^The component is initializing
  | Finalize              -- ^The component is shutting down
  | Submit Event          -- ^The user has pressed Submit
  | Cancel                -- ^Cancel the form
  | Define Operation      -- ^An external component want to change operational mode of the component
  | Input (State->State)  -- ^The user has changed the value in an input field, make a state change

-- |The type of operation to perform on a point, it contains the item key or the
-- latitude and longitude for a new item
data Operation  = UpdatePOI String      -- ^Update the POI
                | ViewPOI String        -- ^Readonly the POI
                | AddPOI Number Number  -- ^Add a new POI
derive instance eqOperation :: Eq Operation

-- |A map that holds a group of attributes
type AttributeGroup = Map.Map String (Array AttributeValue)

-- |A ma that holds the change to an attribute
type AttributeChangeMap = Map.Map String AttributeChange

-- | State for the component
type State =  { alert::Maybe String               -- ^The alert for the component
                , operation::Operation            -- ^What operational mode the component is in
                , item::Maybe Item                -- ^The item
                , attributes::AttributeGroup          -- Grouped attributes not set on an item
                , itemAttributes::AttributeGroup      -- Grouped attrbiutes set on an item
                , attributeChange::AttributeChangeMap  -- Change to an attribute
              }

-- | Initial state contains the operation and the attributes
initialState  :: Operation  -- ^The item key if any
              -> State      -- ^ The state
initialState o =  { alert: Nothing
                    , item: Nothing
                    , operation: o
                    , attributeChange: Map.empty
                    , attributes: Map.empty
                    , itemAttributes: Map.empty
                  }

-- | The component definition
component :: forall r q m. MonadAff m
          => ManageNavigation m
          => MonadAsk r m
          => ManageEntity m
          => ManageItem m
          => H.Component HH.HTML q Operation Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , finalize = Just Finalize
              , receive = Just <<< Define
              }
    }

-- |Updates the state based on the operation
updateState :: forall r m . MonadAff m
            => ManageNavigation m
            => ManageEntity m
            => ManageItem m
            => MonadAsk r m
            => H.HalogenM State Action () Output m Unit -- ^Updated state
updateState = do
  state <- H.get

  -- Fetch the raw data
  a <- queryAttributes >>= evaluateResult AuthenticationError
  av <- (_queryItemAttributes state.operation) >>= (evaluateResult AuthenticationError)
  i <- (_queryItem state.operation) >>= (evaluateResult AuthenticationError)

  -- Construct the mappings and attribute lists
  H.modify_ $ _ {
    attributes = group $ diffF same (fromMaybe [] a) (fromMaybe [] av)
    , item = i
    , itemAttributes = group $ fromMaybe [] av}

  where
    
    -- |Make a group map out of a list of values
    group::Array AttributeValue->AttributeGroup
    group lav = foldr addToMap Map.empty lav

    -- |Add a value to a group map
    addToMap::AttributeValue->AttributeGroup->AttributeGroup
    addToMap av ag = Map.insert av.group ((fromMaybe [] (Map.lookup av.group ag)) <> [av]) ag

    -- |Returns true if the two attribute values have equal keys
    same::AttributeValue->AttributeValue->Boolean
    same a b = a.attributeId == b.attributeId

    -- |Remove all of the elements in the first array that exists in the second array
    -- based on an equality function
    diffF :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
    diffF f = foldr (deleteBy f)

    -- |Query for the items attributes based on the operation
    _queryItemAttributes (UpdatePOI k) = queryItemAttributes k
    _queryItemAttributes (ViewPOI k) = queryItemAttributes k
    _queryItemAttributes (AddPOI _ _) = pure $ Right []

    -- |Query for the item based on the operation, or create a new one if
    -- it is an add operation
    _queryItem (UpdatePOI k) = queryItem k
    _queryItem (ViewPOI k) = queryItem k
    _queryItem (AddPOI la lo) = do
      now <- H.liftEffect $ nowDateTime
      uuid <- H.liftEffect $ genUUID
      pure $ Right { id        : Nothing
                    , name        : ""
                    , guid        : toString uuid
                    , created     : ISO now
                    , description : ""
                    , source      : Human
                    , modifier    : Static
                    , approval    : Waiting
                    , latitude    : la
                    , longitude   : lo
                    , distance    : Nothing
                    , positive    : Nothing
                    , negative    : Nothing
                    , unknown     : Nothing
                  }

-- |Creates a HTML element for an input box of a text type
displayLocation ::  forall p. Maybe Coordinate.Coordinate
          -> HH.HTML p Action         -- ^The HTML element
displayLocation c =
  HH.div [ css "form-group" ] [
    HH.label [ HP.for "Coordinate" ] [ HH.text "Coordinate" ]
    , HH.div [css "input-group"] [
        HH.input ([ css "form-control"
          , HP.title "The POI Coordinate"
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HP.id_ "Latitude"
          , HP.type_ HP.InputText
          , HPA.label "Coordinate"
          , HP.placeholder "Coordinate"
          , HP.disabled true
          ] <> catMaybes [(HP.value <<< Coordinate.toStringHDMS') <$> c])
    ]
  ]

-- |Creates a HTML element for an input box of a text type
inputName ::  forall p. Maybe String  -- ^The value
          -> HH.HTML p Action         -- ^The HTML element
inputName v =
  HH.div [ css "form-group" ] [
    HH.label [ HP.for "Name" ] [ HH.text "Name" ]
    , HH.div [css "input-group"] [
        HH.input ([ css "form-control"
          , HP.title "A name for the point of interest"
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HP.id_ "Name"
          , HP.type_ HP.InputText
          , HPA.label "Name"
          , HP.placeholder "Name"
          , HE.onValueChange \i -> Just $ Input (change i)] <> catMaybes [HP.value <$> v])
        ]
  ]
  where
    change::String->State->State
    change c st = st { item = (_ { name = c }) <$> st.item }

-- |Creates a HTML element for an multiline input box of a text type.
inputDescription  :: forall p. Maybe String -- ^The value
                  -> HH.HTML p Action -- ^The HTML element
inputDescription val =
  HH.div [ css "form-group" ]
    [ HH.label [ HP.for "Description" ] [ HH.text "Description" ]
    , HH.textarea
        ([ css "form-control"
        , HP.title "A description of the point of interest"
        , prop "data-toggle" "tooltip"
        , prop "data-placement" "top"
        , HP.id_ "Description"
        , HP.rows 5
        , HPA.label "Description"
        , HP.placeholder "Description"
        , HE.onValueChange \v -> Just $ Input (change v)
        ] <> catMaybes [HP.value <$> val])
    ]

  where

    change::String->State->State
    change v st = st { item = (_ { description = v }) <$> st.item }

-- |Generate HTML form for a complete map of groups
groupMapInput :: forall p. AttributeGroup -- ^The map of groups of attributes
              -> Array (HH.HTML p Action)    -- ^The HTML element
groupMapInput ag = List.foldr (generate ag) [] (Map.keys ag)
  where

    -- |Generate and accumulate HTML snippets
    generate::forall q. AttributeGroup->String->Array (HH.HTML q Action)->Array (HH.HTML q Action)
    generate grp key acc = acc <> [groupInput key $ fromMaybe [] $ Map.lookup key grp]

-- |Generate a HTML snippet for a group of attributes
groupInput :: forall p. String    -- ^The name of the group
      -> Array AttributeValue     -- ^The attribute value to geneate an input box for
      -> HH.HTML p Action         -- ^The HTML element
groupInput g lav =
  HH.div [ css "form-group"]
    ([HH.h4 [css "mt-3"] [HH.text g]] <> (attributeInput <$> lav))

-- |Creates a HTML snippet for an attribute and its value
attributeInput  :: forall p. AttributeValue -- ^The attribute value to geneate an input box for
                -> HH.HTML p Action         -- ^The HTML element
attributeInput iav =
  HH.div [ css "form-group" ]
    [ HH.label [ HP.for iav.name ] [ HH.text iav.displayName ]
    , inputField iav]

  where

    -- Creates an input field based on attribute type
    inputField av = case av.typeof of
      BooleanType ->
        HH.select
          ([ css "form-control"
          , HP.id_ av.name
          , HP.title av.description
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HPA.label av.name
          , HE.onValueChange \i -> Just $ Input (change i av)
          ] <> catMaybes [HP.value <$> av.value])
          [ HH.option [] [ HH.text "" ]
          , HH.option [] [ HH.text "Yes" ]
          , HH.option [] [ HH.text "No" ]
          ]
      NumberType ->
        HH.div [css "input-group"] [
          HH.input ([ css "form-control"
            , HP.id_ av.name
            , HP.type_ HP.InputNumber
            , HP.title av.description
            , prop "data-toggle" "tooltip"
            , prop "data-placement" "top"
            , HPA.label av.name
            , HP.placeholder av.displayName
            , HE.onValueChange \i -> Just $ Input (change i av)] <> catMaybes [HP.value <$> av.value])
          , HH.div [css "input-group-append"] [HH.span [css "input-group-text"] [HH.text av.unit]]]
      TextType ->
        HH.input ([ css "form-control"
          , HP.title av.description
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HP.id_ av.name
          , HP.type_ HP.InputText
          , HPA.label av.name
          , HP.placeholder av.displayName
          , HE.onValueChange \i -> Just $ Input (change i av)] <> catMaybes [HP.value <$> av.value])

    -- Adds a change order for an attribute whenever it has changed
    change::String->AttributeValue->State->State
    change v cav st = st { attributeChange = 
      Map.insert (fromMaybe "" cav.attributeId) {attributeValueId: cav.attributeValueId
                                                , attributeId: cav.attributeId
                                                , value: Just v } st.attributeChange }

-- | Render the point page
render  :: forall m . MonadAff m
  => State                        -- ^The components state
  -> H.ComponentHTML Action () m  -- ^The components HTML
render state =
  HH.div
    [ css "container-fluid ha-point" ]
    [ HH.form [ css "ha-form-point", HE.onSubmit (Just <<< Submit) ]
        ([ HH.h1 [ css "mt-3" ] [ HH.text "POI Information" ]
        , inputName $ _.name <$> state.item
        , displayLocation $ validate $ catMaybes $ [(_.latitude <$> state.item), (_.longitude <$> state.item)]
        , inputDescription $ _.description <$> state.item
        , HH.h2 [css "mt-3"] [HH.text "Current attributes"]]
        <> (groupMapInput state.itemAttributes) <>
        [ HH.h2 [css "mt-3"] [HH.text "Available attributes"]]
        <> (groupMapInput state.attributes) <>        
        [
          HH.button [ css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit ] [ HH.text 
            (case state.operation of
              ViewPOI _ -> "Ok"
              AddPOI _ _ -> "Create"
              UpdatePOI _ -> "Update") ]
          , HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton
            , HE.onClick \_->Just Cancel  ] [ HH.text "Cancel"]
        ])
    ]
  where

    validate a = case length a of
      2 -> Just a
      _ -> Nothing

-- | Handles all actions for the login component
handleAction  ::  forall r m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Action
              -> H.HalogenM State Action () Output m Unit  -- ^ The handled action

-- |Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize Point Component"
  H.liftEffect enableTooltips
  updateState

-- |Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Point Component"

handleAction Cancel = do
  H.raise Submitted

-- |Form is submitted
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  H.liftEffect $ log "Point Form submitted"
  state <- H.get
  case state.operation of
    ViewPOI _ -> do
      H.liftEffect $ log "ViewPOI is readonly!"
    UpdatePOI _ -> do
      it <- join <$> (sequence $ updateItem <$> state.item)
      u <- sequence $ updateItemAttributes <$> (join (_.id <$> it)) <*> Just (clean <$> (Map.toUnfoldable state.attributeChange))
      H.liftEffect $ log "Update an item"
    AddPOI _ _ -> do
      it <- join <$> (sequence $ addItem <$> state.item)
      u <- sequence $ updateItemAttributes <$> (join (_.id <$> it)) <*> Just (clean <$> (Map.toUnfoldable state.attributeChange))
      H.liftEffect $ log "Add a new item"

  -- Tell the parent that the form has submitted
  H.raise Submitted

    where

      -- Extract the AttributeChange from the tuple
      clean::Tuple String AttributeChange->AttributeChange
      clean (Tuple _ ac) = ac

-- |The input field has changed, we need to save it
handleAction (Input f) = do
  H.liftEffect $ log "Input changed"
  H.modify_ f

-- |It has come a new input, we need to update the state in the same manner as we
-- do for Initialize, but only if the operation has changed.
handleAction (Define i) = do
  H.liftEffect $ log $ "Input received"
  state <- H.get
  when (state.operation /= i) $ do
    H.put $ state {operation = i}
    updateState
