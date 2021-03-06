-- |
-- | The login component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Component.Login
  ( component
  , Output(..)
  , Slot(..)) where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..), fromMaybe)

-- Control Monad
import Control.Monad.Reader.Trans (class MonadAsk)

-- Effects
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Events as HE

-- Web imports
import Web.Event.Event (Event)
import Web.Event.Event as Event

-- Our own stuff
import Accessibility.Data.Route (Page(..))
import Accessibility.Component.HTML.Utils (css)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Authenticate
  (UserInfo(..)
  , Authenticate(..)
  , class ManageAuthentication
  , login)

-- | Slot type for the Login component
type Slot p = forall q . H.Slot q Output p

-- | Output possible to send out from the login component
data Output = SetUser (Maybe UserInfo)    -- | A login or logout event
  | Alert (Maybe String)                          -- | An alert

-- | State for the component
type State =  { alert     ::Maybe String      -- ^ The alert text
              , username  ::Maybe String  -- ^ The username as entered
              , password  ::Maybe String  -- ^ The password as entered
              }

-- | Initial state is no logged in user
initialState  :: forall i . i -- ^ Initial input
              -> State        -- ^ The state
initialState _ =  { alert     : Nothing
                  , username  : Nothing
                  , password  : Nothing
                  }

-- | Internal form actions
data Action = Submit Event          -- ^ Submit of the user
            | Input (State->State)  -- ^ The text boxes has a value

-- | The component definition
component :: forall r q i m . MonadAff m
          => ManageAuthentication m
          => ManageNavigation m
          => MonadAsk r m
          => H.Component q i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- | Render the alert
render  :: forall m . MonadAff m
        => State                        -- ^ The state to render
        -> H.ComponentHTML Action () m  -- ^ The components HTML
render state = HH.div
               [css "ha-login"]
               [              
                   HH.form
                   [css "ha-form-login", HE.onSubmit Submit]
                   [HH.h1 [css "mt-3"] [HH.text "Login"],
                   HH.div
                    [css "form-group"]
                    [HH.label [HP.for "username"] [HH.text "Username"],
                     HH.input [css "form-control", HP.value $ fromMaybe "" state.username, HP.id "username", HP.type_ HP.InputText,
                               HPA.label "username", HP.placeholder "Your username",
                               HE.onValueChange \v -> Input (\st -> st { username = Just v})]
                    ],
                    HH.div
                    [css "form-group"]
                    [HH.label [HP.for "password"] [HH.text "Password"], 
                     HH.input [css "form-control", HP.value $ fromMaybe "" state.password, HP.id "password", HP.type_ HP.InputPassword,
                               HPA.label "password", HP.placeholder "Your password",
                               HE.onValueChange \v -> Input (\st  -> st { password = Just v })]
                    ],
                   HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit] [HH.text "Login"]
                   ]
                  ]

-- | Handles all actions for the login component
handleAction  :: forall r m . MonadAff m
              => ManageAuthentication m
              => ManageNavigation m
              => MonadAsk r m
              => Action                                     -- ^ The action to handle
              -> H.HalogenM State Action () Output m Unit  -- ^ The handled action

-- | Submit => Whenever the Login button is pressed, it will generate a submit message
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  state <- H.modify $ _ {alert = Nothing}

  H.liftEffect $ log $ show state
  userInfo <- login $ Authenticate { username: fromMaybe "" state.username
                                   , password: fromMaybe "" state.password}  
  case userInfo of
    Nothing -> do
      H.put state {alert = Just "Wrong credentials!"}
      H.raise (SetUser Nothing)
    Just ui@(UserInfo val) -> do
      H.liftEffect $ log $ "FORM: Logged in user " <> val.username
      H.raise (SetUser $ Just ui)
      H.liftEffect $ log $ "FORM: Raised setuser"
      gotoPage Home
      H.liftEffect $ log $ "FORM: Goto Page Home"

  -- Raise any alerts    
  (Alert <$> H.gets _.alert) >>= H.raise

-- | Input f => Whenever the textbox entry is done, i.e. by leaving the box or pressing another control it generates a
-- | Input f message, where f is the function that operates on the state to save the new value. It is here we should
-- | perhaps check for format of the input etc.
handleAction (Input f) = do
  H.modify_ f
