-- |
-- | The point component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessibility.Component.Point (component, Operation(..), Slot(..)) where

-- Language imports
import Prelude

-- Data imports
import Data.Array (catMaybes, deleteBy)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.DateTime.ISO (ISO(..))
import Data.UUID (genUUID, toString)

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
import Accessibility.FFI.Utils (enableTooltips)

-- Our own imports
import Accessibility.Component.HTML.Utils (css, prop)
import Accessibility.Interface.Entity (class ManageEntity)
import Accessibility.Interface.Item (
  class ManageItem
  , AttributeType(..)
  , AttributeValue
  , Item
  , ItemApproval(..)
  , ItemSource(..)
  , ItemModifier(..)
  , queryItem
  , queryAttributes
  , queryItemAttributes)
import Accessibility.Interface.Navigate (class ManageNavigation)

-- | Slot type for the Login component
type Slot p = forall q. H.Slot q Void p

-- |The input type, it contains the item key (Left) or the lola for a new (Right)
data Operation = UpdatePOI String -- ^Update the POI
  | ViewPOI String                -- ^Readonly the POI
  | AddPOI Number Number          -- ^Add a new POI

derive instance eqOperation :: Eq Operation

-- | State for the component
type State =  { alert::Maybe String               -- ^The alert for the component
                , operation::Operation            -- ^What operational mode the component is in
                , item::Maybe Item                -- ^The item
                , attrs:: Array AttributeValue    -- ^Attributes that are not part of the item
                , itemAttrs::Array AttributeValue -- ^Attributes that are part of the item
              }

-- | Initial state is no logged in user
initialState  :: Operation  -- ^The item key if any
              -> State      -- ^ The state
initialState i =  { alert: Nothing
                    , item: Nothing
                    , operation: i
                    , attrs: []
                    , itemAttrs: []
                  }

-- | Internal form actions
data Action = Initialize  -- ^The component is initializing
  | Finalize              -- ^The component is shutting down
  | Submit Event          -- ^The user has pressed Submit
  | Define Operation      -- ^An external component want to change operational mode of the component
  | Input                 -- ^The user has changed the value in an input field

-- | The component definition
component :: forall r q o m. MonadAff m
          => ManageNavigation m
          => MonadAsk r m
          => ManageEntity m
          => ManageItem m
          => H.Component HH.HTML q Operation o m
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

-- |The component alert HTML
nearbyAlert :: forall p i. Maybe String
            -> HH.HTML p i
nearbyAlert (Just t) = HH.div [ css "alert alert-danger" ] [ HH.text $ t ]
nearbyAlert Nothing = HH.div [] []

-- |Creates a HTML element for an input box of a text type
inputText ::  forall p. String  -- ^The name of the field
          -> Maybe String       -- ^The value of the field, if any
          -> HH.HTML p Action   -- ^The HTML element
inputText name v =
  HH.div [ css "form-group" ] [
    HH.label [ HP.for name ] [ HH.text name ]
    , HH.div [css "input-group"] [
        HH.input ([ css "form-control"
          , HP.title "This is a tooltip, yeah yeah yeah!!!!"
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HP.id_ name
          , HP.type_ HP.InputText
          , HPA.label name
          , HP.placeholder name
          , HE.onValueChange \_ -> Just $ Input] <> catMaybes [HP.value <$> v])
        ]
  ]

-- |Creates a HTML element for an input box of a number type
inputNumber ::  forall p. String  -- ^The name of the field
            -> Maybe String       -- ^The value of the field, if any
            -> String             -- ^The unit of the field
            -> HH.HTML p Action   -- ^The HTML element
inputNumber name val unit =
  HH.div [ css "form-group" ] [
      HH.label [ HP.for name ] [ HH.text name ]
      , HH.div [css "input-group"] [
          HH.input ([ css "form-control"
            , HP.id_ name
            , HP.type_ HP.InputNumber
            , HPA.label name
            , HP.placeholder name
            , HE.onValueChange \v -> Just $ Input] <> catMaybes [HP.value <$> val])
          , HH.div [css "input-group-append"] [
              HH.span [css "input-group-text"] [HH.text unit]]]
    ]

-- |Creates a HTML element for an input box of a tet type but is a multiline
inputTextArea :: forall p. String -- ^Name of the field
              -> Maybe String     -- ^The value of the field, if any
              -> HH.HTML p Action -- ^The HTML element
inputTextArea name val =
  HH.div [ css "form-group" ]
    [ HH.label [ HP.for name ] [ HH.text name ]
    , HH.textarea
        ([ css "form-control"
        , HP.id_ name
        , HP.rows 5
        , HPA.label name
        , HP.placeholder name
        , HE.onValueChange \v -> Just $ Input
        ] <> catMaybes [HP.value <$> val])
    ]

-- |Creates a HTM element for an input box of type Boolean (Yes/No)
inputYesNo  :: forall p. String -- ^The name of the field
            -> Maybe String     -- ^The value of the field, if any
            -> HH.HTML p Action -- ^The HTML element
inputYesNo name val =
  HH.div [ css "form-group" ]
    [ HH.label [ HP.for name ] [ HH.text name ]
    , HH.select
        ([ css "form-control"
        , HP.id_ name
        , HPA.label name
        , HE.onValueChange \v -> Just $ Input
        ] <> catMaybes [HP.value <$> val])
        [ HH.option [] [ HH.text "Yes" ]
        , HH.option [] [ HH.text "No" ]
        ]
    ]

-- |Creates a HTML element based on the attribute value
input :: forall p. AttributeValue -- ^The attribute value to geneate an input box for
      -> HH.HTML p Action         -- ^The HTML element
input av = case av.typeof of
  TextType -> inputText av.name av.value
  BooleanType -> inputYesNo av.name av.value
  NumberType -> inputNumber av.name av.value av.unit

-- | Render the nearby page
render  :: forall m . MonadAff m
  => State                        -- ^The components state
  -> H.ComponentHTML Action () m  -- ^The components HTML
render state =
  HH.div
    [ css "container-fluid" ]
    [ HH.div [ css "row" ]
        [ HH.div [ css "col-xs-12 col-md-12" ]
            [ nearbyAlert state.alert
            ]
        ]
    , HH.form [ css "form-signin", HE.onSubmit (Just <<< Submit) ]
        ([ HH.h1 [ css "mt-3" ] [ HH.text "POI Information" ]
        , inputText "Name" (_.name <$> state.item)
        , inputTextArea "Description" (_.description <$> state.item)
        , HH.h2 [css "mt-3"] [HH.text "Current attributes"]]
        <> (input <$> state.itemAttrs) <>
        [ HH.h2 [css "mt-3"] [HH.text "Available attributes"]]
        <> (input <$> state.attrs) <>
        [HH.button [ css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit ] [ HH.text "Save" ]
        ])
    ]

-- |Updates the state based on the operation
updateState :: forall r o m . MonadAff m
            => ManageNavigation m
            => ManageEntity m
            => ManageItem m
            => MonadAsk r m
            => H.HalogenM State Action () o m Unit -- ^Updated state
updateState = do
  state <- H.get
  a <- queryAttributes
  av <- _queryItemAttributes state.operation
  H.liftEffect $ log $ show av
  i <- _queryItem state.operation
  H.liftEffect $ log $ show i
  H.put state { attrs = diffF same (fromMaybe [] a) (fromMaybe [] av)
    , item = i
    , itemAttrs = fromMaybe [] av}

  where

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
    _queryItemAttributes (AddPOI _ _) = pure Nothing

    -- |Query for the item based on the operation, or create a new one if
    -- it is an add operation
    _queryItem (UpdatePOI k) = queryItem k
    _queryItem (ViewPOI k) = queryItem k
    _queryItem (AddPOI la lo) = do
      now <- H.liftEffect $ nowDateTime
      uuid <- H.liftEffect $ genUUID
      pure $ Just { id        : Nothing
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
                  }

-- | Handles all actions for the login component
handleAction  ::  forall r o m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => Action
              -> H.HalogenM State Action () o m Unit  -- ^ The handled action

-- |Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize Point Component"
  H.liftEffect enableTooltips
  updateState

-- |Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Point Component"

-- |Form is submitted
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  H.liftEffect $ log "Point Form submitted"

-- |The input field has changed, we need to save it
handleAction Input = do
  state <- H.get
  H.liftEffect $ log "Input changed"

-- |It has come a new input, we need to update the state in the same manner as we
-- do for Initialize, but only if the operation has changed.
handleAction (Define i) = do
  H.liftEffect $ log $ "Input received"
  state <- H.get
  when (state.operation /= i) $ H.put $ state {operation = i}
  updateState
