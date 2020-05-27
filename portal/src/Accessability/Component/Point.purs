-- |
-- | The point component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessability.Component.Point where

-- Language imports
import Prelude
import Accessability.Component.HTML.Utils (css, style)
import Accessability.Interface.Entity (class ManageEntity, queryEntities, Entity(..))
import Accessability.Interface.Item (
  class ManageItem,
  queryItems,
  queryAttributes,
  queryItemAttributes,
  AttributeValue(..),
  AttributeChange(..),
  AttributeType(..),
  Item)
import Accessability.Interface.Navigate (class ManageNavigation)
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array ((!!), catMaybes, deleteBy)
import Data.Foldable (sequence_, foldr)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
-- Web imports
import Web.Event.Event (Event)
import Web.Event.Event as Event

-- | Slot type for the Login component
type Slot p
  = ∀ q. H.Slot q Void p

-- | State for the component
type State
  = { alert::Maybe String,
      key::Maybe String,
      attrs:: Array AttributeValue,
      itemAttrs::Array AttributeValue }

-- |The input type, it contains the item key, if any
type Input = Maybe String

-- | Initial state is no logged in user
initialState ∷ Input -- ^The item key if any
  -> State                  -- ^ The state
initialState k = {  alert: Nothing,
                    key: k,
                    attrs: [],
                    itemAttrs: []}

-- | Internal form actions
data Action
  = Initialize
  | Finalize
  | Submit Event
  | HandleInput Input
  | Input

-- | The component definition
component ∷
  ∀ r q o m.
  MonadAff m ⇒
  ManageNavigation m =>
  MonadAsk r m =>
  ManageEntity m =>
  ManageItem m ⇒
  H.Component HH.HTML q Input o m
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
              , receive = Just <<< HandleInput
              }
    }

nearbyAlert :: forall p i. Maybe String -> HH.HTML p i
nearbyAlert (Just t) = HH.div [ css "alert alert-danger" ] [ HH.text $ t ]

nearbyAlert Nothing = HH.div [] []

-- |Creates a HTML element for an input box of a text type
inputText ∷ forall p. String  -- ^The name of the field
  -> Maybe String             -- ^The value of the field, if any
  -> HH.HTML p Action         -- ^The HTML element
inputText name v =
  HH.div [ css "form-group" ] [
    HH.label [ HP.for name ] [ HH.text name ]
    , HH.div [css "input-group"] [
        HH.input ([ css "form-control"
          , HP.id_ name
          , HP.type_ HP.InputText
          , HPA.label name
          , HP.placeholder name
          , HE.onValueChange \_ -> Just $ Input] <> catMaybes [HP.value <$> v])
        ]
  ]

-- |Creates a HTML element for an input box of a number type
inputNumber ∷ forall p. String  -- ^The name of the field
  -> Maybe String                     -- ^The value of the field, if any
  -> String                     -- ^The unit of the field
  -> HH.HTML p Action           -- ^The HTML element
inputNumber name v unit =
  HH.div [ css "form-group" ] [
      HH.label [ HP.for name ] [ HH.text name ]
      , HH.div [css "input-group"] [
          HH.input ([ css "form-control"
            , HP.id_ name
            , HP.type_ HP.InputNumber
            , HPA.label name
            , HP.placeholder name
            , HE.onValueChange \v -> Just $ Input] <> catMaybes [HP.value <$> v])
          , HH.div [css "input-group-append"] [
              HH.span [css "input-group-text"] [HH.text unit]]]
    ]

-- |Creates a HTML element for an input box of a tet type but is a multiline
inputTextArea ∷ forall p. String  -- ^Name of the field
  -> Maybe String                 -- ^The value of the field, if any
  -> HH.HTML p Action             -- ^The HTML element
inputTextArea name v =
  HH.div [ css "form-group" ]
    [ HH.label [ HP.for name ] [ HH.text name ]
    , HH.textarea
        ([ css "form-control"
        , HP.id_ name
        , HP.rows 5
        , HPA.label name
        , HP.placeholder name
        , HE.onValueChange \v -> Just $ Input
        ] <> catMaybes [HP.value <$> v])
    ]

-- |Creates a HTM element for an input box of type Boolean (Yes/No)
inputYesNo ∷ forall p. String -- ^The name of the field
  -> Maybe String             -- ^The value of the field, if any
  -> HH.HTML p Action         -- ^The HTML element
inputYesNo name v =
  HH.div [ css "form-group" ]
    [ HH.label [ HP.for name ] [ HH.text name ]
    , HH.select
        ([ css "form-control"
        , HP.id_ name
        , HPA.label name
        , HE.onValueChange \v -> Just $ Input
        ] <> catMaybes [HP.value <$> v])
        [ HH.option [] [ HH.text "Yes" ]
        , HH.option [] [ HH.text "No" ]
        ]
    ]

-- |Creates a HTML element based on the attribute value
input :: forall p. AttributeValue -- ^The attribute value to geneate an input box for
  -> HH.HTML p Action             -- ^The HTML element
input av = case av.typeof of
  TextType -> inputText av.name av.value
  BooleanType -> inputYesNo av.name av.value
  NumberType -> inputNumber av.name av.value av.unit

-- | Render the nearby page
render ∷
  ∀ m.
  MonadAff m ⇒
  State ->
  H.ComponentHTML Action () m -- ^ The components HTML
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
        , inputText "Name" Nothing
        , inputTextArea "Description" Nothing
        , HH.h2 [css "mt-3"] [HH.text "Current attributes"]]
        <> (input <$> state.itemAttrs) <>
        [ HH.h2 [css "mt-3"] [HH.text "Available attributes"]]
        <> (input <$> state.attrs) <>
        [HH.button [ css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit ] [ HH.text "Save" ]
        ])
    ]

-- | Handles all actions for the login component
handleAction ∷
  ∀ r o m.
  MonadAff m =>
  ManageNavigation m =>
  ManageEntity m =>
  ManageItem m =>
  MonadAsk r m =>
  Action ->
  H.HalogenM State Action () o m Unit -- ^ The handled action
-- | Initialize action
handleAction Initialize = do
  state <- H.get
  H.liftEffect $ log "Initialize Point Component"
  H.liftEffect $ log $ show state
  a <- queryAttributes
  av <- queryItemAttributes "0000000000000001"
  H.put state { attrs = diffF same (fromMaybe [] a) (fromMaybe [] av), itemAttrs = fromMaybe [] av}
  where

    same::AttributeValue->AttributeValue->Boolean
    same a b = a.attributeId == b.attributeId

    diffF :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
    diffF f = foldr (deleteBy f)

-- | Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize Point Component"

handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  H.liftEffect $ log "Point Form submitted"

handleAction Input = do
  state <- H.get
  H.liftEffect $ log $ show state
  H.liftEffect $ log "Input changed"

handleAction (HandleInput input) = do
  H.liftEffect $ log $ "Input received " <> show input
