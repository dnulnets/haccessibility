-- |
-- | The login component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Component.Login where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)

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
import Accessibility.Component.HTML.Utils (css, style)
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Authenticate (UserInfo(..),
                                    Authenticate(..),
                                    class ManageAuthentication,
                                    login)

-- | Slot type for the Login component
type Slot p = ∀ q . H.Slot q Message p

-- | Messages possible to send out from the login component
data Message = SetUserMessage (Maybe UserInfo)   -- | A login or logout event             

-- | State for the component
type State = {  alert::Maybe String,   -- ^ The alert text
                username∷Maybe String, -- ^ The username as entered
                password∷Maybe String -- ^ The password as entered
              }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i   -- ^ Initial input
  → State               -- ^ The state
initialState _ = { alert : Nothing,
                   username : Nothing,
                   password : Nothing}

-- | Internal form actions
data Action = Submit Event        -- ^ Submit of the user
            | Input (State→State) -- ^ The text boxes has a value

-- | The component definition
component ∷ ∀ r q i m . MonadAff m
            ⇒ ManageAuthentication m
            ⇒ ManageNavigation m
            ⇒ MonadAsk r m
            ⇒ H.Component HH.HTML q i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

loginAlert::forall p i . Maybe String -> HH.HTML p i
loginAlert t = HH.b [css "", style $ ("color:red;visibility:" <> (maybe "hidden" (\_->"visible") t))] 
  [HH.text $ fromMaybe "" t]

--loginAlert t = HH.div [css "alert alert-danger alert-signin", HPA.role "alert", style "visibility: hidden"] [HH.text t]

-- | Render the alert
render ∷ ∀ m . MonadAff m ⇒ State -- ^ The state to render
  → H.ComponentHTML Action () m   -- ^ The components HTML
render state = HH.div
               []
               [              
                   HH.form
                   [css "form-signin", HE.onSubmit (Just <<< Submit)]
                   [HH.h1 [css "mt-3"] [HH.text "Login"],
                   loginAlert state.alert,
                   HH.div
                    [css "form-group"]
                    [HH.label [HP.for "username"] [HH.text "Username"],
                     HH.input [css "form-control", HP.value $ fromMaybe "" state.username, HP.id_ "username", HP.type_ HP.InputText,
                               HPA.label "username", HP.placeholder "Your username",
                               HE.onValueChange \v -> Just $ Input (\st -> st { username = Just v})]
                    ],
                    HH.div
                    [css "form-group"]
                    [HH.label [HP.for "password"] [HH.text "Password"], 
                     HH.input [css "form-control", HP.value $ fromMaybe "" state.password, HP.id_ "password", HP.type_ HP.InputPassword,
                               HPA.label "password", HP.placeholder "Your password",
                               HE.onValueChange \v -> Just $ Input (\st  -> st { password = Just v })]
                    ],
                   HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit] [HH.text "Login"]
                   ]
                  ]

-- | Handles all actions for the login component
handleAction ∷ ∀ r m . MonadAff m
            ⇒ ManageAuthentication m
            ⇒ ManageNavigation m
            => MonadAsk r m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () Message m Unit -- ^ The handled action

-- | Submit => Whenever the Login button is pressed, it will generate a submit message
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  state <- H.get
  H.liftEffect $ log $ show state
  userInfo <- login $ Authenticate { username: fromMaybe "" state.username
                                   , password: fromMaybe "" state.password}
  case userInfo of
    Nothing -> do
      H.put state {alert = Just "Wrong credentials"}
      H.raise (SetUserMessage Nothing)
    Just ui@(UserInfo val) -> do
      H.liftEffect $ log $ "Logged in user " <> val.username
      H.put state {alert = Nothing}
      H.raise (SetUserMessage $ Just ui)
      gotoPage Home
      
-- | Input f => Whenever the textbox entry is done, i.e. by leaving the box or pressing another control it generates a
-- | Input f message, where f is the function that operates on the state to save the new value. It is here we should
-- | perhaps check for format of the input etc.
handleAction (Input f) = do
  H.modify_ f
