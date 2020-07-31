-- |
-- | The point component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessibility.Component.UserProperty (component, Slot(..), Output(..)) where

-- Language imports
import Prelude

-- Data imports
import Data.Array (catMaybes, deleteBy)
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Map as Map
import Data.List as List

-- Monad imports
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Alt ((<|>))

-- Effect imports
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

-- Our own imports
import Accessibility.Util.Result (evaluateResult)
import Accessibility.Component.HTML.Utils (css, prop, enableTooltips)
import Accessibility.Interface.Entity (class ManageEntity)
import Accessibility.Interface.User (class ManageUser, Operation(..), UserProperty, UserPropertyChange, displayToOperation, operationToDisplay, queryUserProperties, updateUserProperties)
import Accessibility.Interface.Item (class ManageItem, AttributeType(..), AttributeValue, queryAttributes)
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
  | Input (State->State)  -- ^The user has changed the value in an input field, make a state change

-- |A map that holds a group of properties
type UserPropertyGroup = Map.Map String (Array UserProperty)

-- |A map the holds all changes, the AttributeId is the key
type UserPropertyChangeMap = Map.Map String UserPropertyChange

-- | State for the component
type State =  { alert::Maybe String               -- ^The alert for the component
                , properties::UserPropertyGroup
                , userProperties::UserPropertyGroup
                , propertyChange::UserPropertyChangeMap
              }

-- | Initial state contains the operation and the attributes
initialState  :: forall i . i -> State
initialState i =  { alert: Nothing                  
                    , properties: Map.empty
                    , userProperties: Map.empty
                    , propertyChange: Map.empty
                  }

-- | The component definition
component :: forall r q m i. MonadAff m
          => ManageNavigation m
          => MonadAsk r m
          => ManageEntity m
          => ManageItem m
          => ManageUser m
          => H.Component HH.HTML q i Output m
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
              }
    }

-- |Updates the state based on the operation
updateState :: forall r m . MonadAff m
            => ManageNavigation m
            => ManageEntity m
            => ManageItem m
            => ManageUser m
            => MonadAsk r m
            => H.HalogenM State Action () Output m Unit
updateState = do
  state <- H.get

  -- Fetch all properties and the users properties
  a <- queryAttributes >>= evaluateResult AuthenticationError
  av <- (queryUserProperties >>= (evaluateResult AuthenticationError))

  -- Construct the mappings and attribute lists
  H.modify_ $ _ {
    properties = group $ diffF same (fromMaybe [] ((map toUserProperty) <$> a)) (fromMaybe [] av)
    , userProperties = group $ fromMaybe [] av}

  where
    
    -- |Make a UserProperty out of an AttributeValue
    toUserProperty::AttributeValue->UserProperty
    toUserProperty av = { userPropertyId: Nothing
        , name: av.name
        , group: av.group
        , displayName: av.displayName
        , description: av.description
        , typeof: av.typeof
        , unit: av.unit
        , value: Nothing
        , value1: Nothing
        , attributeId: av.attributeId
        , operation: Nothing
        , negate: Nothing
        }

    -- |Make a group map out of a list of values
    group::Array UserProperty->UserPropertyGroup
    group lav = foldr addToMap Map.empty lav

    -- |Add a value to a group map
    addToMap::UserProperty->UserPropertyGroup->UserPropertyGroup
    addToMap av ag = Map.insert av.group ((fromMaybe [] (Map.lookup av.group ag)) <> [av]) ag

    -- |Returns true if the two attribute values have equal keys
    same::UserProperty->UserProperty->Boolean
    same a b = a.attributeId == b.attributeId

    -- |Remove all of the elements in the first array that exists in the second array
    -- based on an equality function
    diffF :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
    diffF f = foldr (deleteBy f)

-- |Creates a HTML element for an input box of a text type
displayEmail ::  forall p. Maybe String
          -> HH.HTML p Action         -- ^The HTML element
displayEmail me =
  HH.div [ css "form-group" ] [
    HH.label [ HP.for "Email" ] [ HH.text "Email" ]
    , HH.div [css "input-group"] [
        HH.input [ css "form-control"
          , HP.title "The users email"
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HP.id_ "Email"
          , HP.type_ HP.InputText
          , HPA.label "Email"
          , HP.placeholder "Email"
          , HP.disabled true
          , HP.value $ fromMaybe "" me
        ]
    ]
  ]

-- |Creates a HTML element for the user name
displayUsername ::  forall p. Maybe String  -- ^The username
          -> HH.HTML p Action         -- ^The HTML element
displayUsername mv =
  HH.div [ css "form-group" ] [
    HH.label [ HP.for "Username" ] [ HH.text "Username" ]
    , HH.div [css "input-group"] [
        HH.input [ css "form-control"
          , HP.title "The username"
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HP.id_ "Username"
          , HP.type_ HP.InputText
          , HPA.label "Username"
          , HP.placeholder "Username"
          , HP.disabled true
          , HP.value $ fromMaybe "" mv
        ]
    ]
  ]

-- |Generate HTML form for a complete map of groups
groupMapInput :: forall p. State->UserPropertyGroup -- ^The map of groups of attributes
              -> Array (HH.HTML p Action)    -- ^The HTML element
groupMapInput state ag = List.foldr (generate state ag) [] (Map.keys ag)
  where

    -- |Generate and accumulate HTML snippets
    generate::forall q. State->UserPropertyGroup->String->Array (HH.HTML q Action)->Array (HH.HTML q Action)
    generate st grp key acc = acc <> [groupInput st key $ fromMaybe [] $ Map.lookup key grp]

-- |Generate a HTML snippet for a group of attributes
groupInput :: forall p . State -> String    -- ^The name of the group
      -> Array UserProperty     -- ^The attribute value to geneate an input box for
      -> HH.HTML p Action         -- ^The HTML element
groupInput state g lav =
  HH.div [ css "form-group"]
    ([HH.h4 [css "mt-3"] [HH.text g]] <> ((propertyInput state) <$> lav))

-- |Creates a HTML snippet for an property its value, unit and operations
propertyInput  :: forall p. State->UserProperty -- ^The attribute value to geneate an input box for
                -> HH.HTML p Action             -- ^The HTML element
propertyInput state iav =
  HH.div [ css "form-group" ]
    ([ HH.label [ HP.for iav.name ] [ HH.text iav.displayName ]] <> (inputField state iav) <> (inputOperation iav))

  where

    -- |Create the input controls depending on the type
    inputOperation av =
      [ HH.div [css "form-group"] [          
          HH.div [css "form-inline mt-2"] [
            HH.label [HP.for ("operation-" <> av.name)] [HH.text "Operation:"]
            , HH.select
              ([ css "form-control ml-1"
              , HP.id_ ("operation-" <> av.name)
              , HP.title ("The operation for " <> av.description)
              , prop "data-toggle" "tooltip"
              , prop "data-placement" "top"
              , HPA.label ("operation" <> av.name)
              , HE.onValueChange \i -> Just $ Input (changeOperation i av)
              ] <> catMaybes [(HP.value <<< operationToDisplay) <$> av.operation])
              (operationChoices av.typeof)
            , HH.div [css "form-group form-check"] [
                HH.input ([ css "form-check-input ml-1"
                  , HP.id_ ("negate-" <> av.name)
                  , HP.type_ HP.InputCheckbox
                  , HP.title ("Negation of the operation " <> av.description)
                  , prop "data-toggle" "tooltip"
                  , prop "data-placement" "top"
                  , HPA.label ("negation " <> av.name)
                  , HE.onChecked \i -> Just $ Input (changeNegate i av)] <> catMaybes [HP.checked <$> av.negate])
              , HH.label [css "form-check-label", HP.for ("negate-" <> av.name)] [HH.text "Negate"]
            ]
          ]
        ]
      ]

    -- Creates an input field based on attribute type
    inputField::State->UserProperty->Array (HH.HTML p Action)
    inputField st up = case up.typeof of

      -- The boolean type is a selcet of Yes or No
      BooleanType ->
        [ HH.select
          ([ css "form-control"
          , HP.id_ up.name
          , HP.title up.description
          , prop "data-toggle" "tooltip"
          , prop "data-placement" "top"
          , HPA.label up.name
          , HE.onValueChange \i -> Just $ Input (changeValue i up)
          ] <> catMaybes [HP.value <$> up.value])
          [ HH.option [] [ HH.text "<no value>" ]
          , HH.option [] [ HH.text "Yes" ]
          , HH.option [] [ HH.text "No" ]
          ]
        ]

      -- A number or can be a range when the operation is "in between"
      NumberType ->
        ([ HH.div [css "input-group"] [
            HH.input ([ css "form-control"
              , HP.id_ up.name
              , HP.type_ HP.InputNumber
              , HP.title up.description
              , prop "data-toggle" "tooltip"
              , prop "data-placement" "top"
              , HPA.label up.name
              , HP.placeholder up.displayName
              , HE.onValueChange \i -> Just $ Input (changeValue i up)] <> catMaybes [HP.value <$> up.value])
            , HH.div [css "input-group-append"] [HH.span [css "input-group-text"] [HH.text up.unit]]
          ]
        ]) <> (maybe [] (\o -> case o of
          OIN ->
            [ HH.div [css "input-group"] [
              HH.input ([ css "form-control mt-1"
                , HP.id_ (up.name <> "1")
                , HP.type_ HP.InputNumber
                , HP.title up.description
                , prop "data-toggle" "tooltip"
                , prop "data-placement" "top"
                , HPA.label up.name
                , HP.placeholder up.displayName
                , HE.onValueChange \i -> Just $ Input (changeValue1 i up)] <> catMaybes [HP.value <$> up.value1])
              , HH.div [css "input-group-append mt-1"] [HH.span [css "input-group-text"] [HH.text up.unit]]]]
          _ -> []
        ) (realOperation st up))
      TextType -> []

    -- |Decide what the real chosen operation is
    realOperation::State->UserProperty->Maybe Operation
    realOperation st up =
        (join (_.operation <$> oper)) <|> up.operation
      where
        oper = join $ Map.lookup <$> up.attributeId <*> (Just st.propertyChange)

    -- |Decide the options for the operations
    operationChoices BooleanType = [ 
      HH.option [] [ HH.text "" ]
      , HH.option [] [ HH.text "=" ]]
    operationChoices TextType = []
    operationChoices NumberType = [
      HH.option [] [ HH.text "" ]
      , HH.option [] [ HH.text "<" ]
      , HH.option [] [ HH.text "<=" ]
      , HH.option [] [ HH.text "=" ]
      , HH.option [] [ HH.text ">=" ]
      , HH.option [] [ HH.text ">" ]
      , HH.option [] [ HH.text "between" ]]

    -- Creates a deafult change based on a user property
    default::UserProperty->UserPropertyChange
    default up = {userPropertyId: up.userPropertyId
                  , attributeId: up.attributeId
                  , value: up.value
                  , operation: up.operation
                  , negate: up.negate
                  , value1: up.value1 }

    -- Adds a change order for an attribute whenever it has changed
    changeValue::String->UserProperty->State->State
    changeValue v up st = st { propertyChange = 
      Map.insert (fromMaybe "" up.attributeId) ((case join (Map.lookup <$> up.attributeId <*> (Just st.propertyChange)) of
                                            Nothing -> (default up) {value = Just v }
                                            Just a -> a {value = Just v } )) st.propertyChange }

    -- Adds a change order for an attribute whenever it has changed
    changeNegate::Boolean->UserProperty->State->State
    changeNegate v up st = st { propertyChange = 
      Map.insert (fromMaybe "" up.attributeId) ((case join (Map.lookup <$> up.attributeId <*> (Just st.propertyChange)) of
                                            Nothing -> (default up) {negate = Just v }
                                            Just a -> a {negate = Just v } )) st.propertyChange }

    -- Adds a change order for an attribute whenever it has changed
    changeValue1::String->UserProperty->State->State
    changeValue1 v up st = st { propertyChange = 
      Map.insert (fromMaybe "" up.attributeId) ((case join (Map.lookup <$> up.attributeId <*> (Just st.propertyChange)) of
                                            Nothing -> (default up) {value1 = Just v }
                                            Just a -> a {value1 = Just v } )) st.propertyChange }

    changeOperation::String->UserProperty->State->State
    changeOperation v up st = st { propertyChange = 
      Map.insert (fromMaybe "" up.attributeId) ((case join (Map.lookup <$> up.attributeId <*> (Just st.propertyChange)) of
                                            Nothing -> (default up) { operation = displayToOperation v }
                                            Just a -> a {operation = displayToOperation v } )) st.propertyChange }

-- | Render the point page
render  :: forall m . MonadAff m
  => State                        -- ^The components state
  -> H.ComponentHTML Action () m  -- ^The components HTML
render state =
  HH.div
    [ css "container-fluid ha-point" ]
    [ HH.form [ css "ha-form-point", HE.onSubmit (Just <<< Submit) ]
        ([ HH.h1 [ css "mt-3" ] [ HH.text "User properties" ]
        , displayUsername $ Just "tomas"
        , displayEmail $ Just "tomas.stenlund@telia.com"
        , HH.h2 [css "mt-3"] [HH.text "Current properties"]]
        <> (groupMapInput state state.userProperties) <>
        [ HH.h2 [css "mt-3"] [HH.text "Available properties"]]
        <> (groupMapInput state state.properties) <>        
        [
          HH.button [ css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit ] [ HH.text 
            "Update" ]
          , HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton
            , HE.onClick \_->Just Cancel  ] [ HH.text "Cancel"]
        ])
    ]

-- | Handles all actions for the login component
handleAction  ::  forall r m . MonadAff m
              => ManageNavigation m
              => ManageEntity m
              => ManageItem m
              => MonadAsk r m
              => ManageUser m
              => Action
              -> H.HalogenM State Action () Output m Unit  -- ^ The handled action

-- |Initialize action
handleAction Initialize = do
  H.liftEffect $ log "Initialize User properties Component"
  H.liftEffect enableTooltips
  updateState

-- |Finalize action
handleAction Finalize = do
  H.liftEffect $ log "Finalize User properties Component"

handleAction Cancel = do
  H.raise Submitted

-- |Form is submitted
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  H.liftEffect $ log "User Properties Form submitted"
  state <- H.get
  u <- sequence $ updateUserProperties <$> (Just (clean <$> (Map.toUnfoldable state.propertyChange)))

  -- Tell the parent that the form has been submitted
  H.raise Submitted

    where

      clean::Tuple String UserPropertyChange->UserPropertyChange
      clean (Tuple _ ac) = ac

-- |The input field has changed, we need to save it
handleAction (Input f) = do
  H.liftEffect $ log "Input changed"
  state <- H.modify f
  H.liftEffect $ log $ show state.propertyChange
