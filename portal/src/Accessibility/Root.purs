-- |
-- | The Root container module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Root (component,
  Query (..)) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))

import Control.Monad.Reader.Trans (class MonadAsk)

import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Events as HE

-- DOM import
import DOM.HTML.Indexed.ButtonType (ButtonType(..))

-- Our own stuff
import Accessibility.Data.Route (Page(..))
import Accessibility.Component.HTML.Utils (css,
                                  style,
                                  prop,
                                  href)
                                  
import Accessibility.Component.Login as Login
import Accessibility.Component.Nearby as Nearby
import Accessibility.Component.Point as Point
import Accessibility.Interface.Navigate (class ManageNavigation, gotoPage)
import Accessibility.Interface.Authenticate (class ManageAuthentication, UserInfo (..))
import Accessibility.Interface.Item (class ManageItem)
import Accessibility.Interface.Entity (class ManageEntity)


-- | The state of the root page
type State = {  userInfo :: Maybe UserInfo -- ^ User information of the logged in user
              , page :: Page }             -- ^ What page to show in the root container

-- | The query that allows us to change page of the root
data Query a = GotoPageRequest Page a

-- | The actions supported by the root page
data Action = SetUserAction  (Maybe UserInfo)   -- ^Sets the user
            | Logout -- ^Logs out the user

-- | The set of slots for the root container
type ChildSlots = ( login ∷ Login.Slot Unit,
                    nearby :: Nearby.Slot Unit,
                    point :: Point.Slot Unit )

_login = SProxy::SProxy "login"
_nearby = SProxy::SProxy "nearby"
_point = SProxy::SProxy "point"

component ∷ ∀ r i o m. MonadAff m
  => ManageAuthentication m
  => ManageNavigation m
  => ManageItem m
  => ManageEntity m
  => MonadAsk r m
  => H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

-- | The root container initial state
initialState ∷ ∀ i. i → State
initialState _ = {  userInfo: Nothing
                  , page: Login }

-- |The navigation bar for the page
navbar∷forall p i . Array (HH.HTML p i) -> HH.HTML p i
navbar html = HH.nav [css "navbar navbar-dark bg-warning fixed-top navbar-expand-md", HPA.role "navigation"] html

-- |The header of the navigation bar
navbarHeader∷forall p i . String -> Array( HH.HTML p i )
navbarHeader header = [HH.button [css "navbar-toggler",
                        HP.type_ ButtonButton,
                        prop "data-toggle" "collapse",
                        prop "data-target" "#navbarCollapse",
                        HPA.expanded "false",
                        HPA.controls "navbarCollapse",
                        HPA.label "Toggle navigation"]
                        [HH.span [css "navbar-toggler-icon"] []],
                      HH.a [css "navbar-brand", href Home]
                        [HH.text header]                                                               
                      ]

-- |The left navigation bar
navbarLeft∷forall p . State -> HH.HTML p Action
navbarLeft state = HH.div [css "collapse navbar-collapse", HP.id_ "navbarCollapse"]
                    [HH.ul [css "navbar-nav mr-auto"] [
                      HH.li [css "nav-item active"] [HH.a [css "nav-link", href Home] [HH.text "Map"]],
                      HH.li [css "nav-item"] [HH.a [css "nav-link", href (Point "0000000000000001")] [HH.text "Add POI"]],
                      HH.li [css "nav-item"] [HH.a [css "nav-link", href Home] [HH.text "Not used"]]
                      ]          
                    ]

-- |The right navigation bar
navbarRight∷forall p . State -> HH.HTML p Action
navbarRight state = HH.a [css "navbar-text", HE.onClick \_ -> Just Logout]
                      [HH.text $ maybe "Not logged in" (\(UserInfo v)->"Logout " <> v.username) state.userInfo]

render ∷ ∀ r m . MonadAff m
  => ManageAuthentication m
  => ManageNavigation m
  => ManageEntity m
  => ManageItem m
  => MonadAsk r m
  => State → H.ComponentHTML Action ChildSlots m
render state = HH.div [] [
  HH.header [] [navbar $ (navbarHeader "Case 3 Prototype") <> [navbarLeft state, navbarRight state]],
  HH.main [css "container", HPA.role "main"][view state.page]]

-- | Render the main view of the page
view ∷ ∀ r m. MonadAff m
       ⇒ ManageAuthentication m
       ⇒ ManageNavigation m
       => ManageEntity m
       => ManageItem m
       ⇒ MonadAsk r m
       ⇒ Page → H.ComponentHTML Action ChildSlots m
view Login = HH.slot _login  unit Login.component  unit (Just <<< loginMessageConv)
view Home =  HH.slot _nearby unit Nearby.component unit absurd
view (Point k) =  HH.slot _point unit Point.component (Point.UpdatePOI k) absurd
view (AddPoint la lo) = HH.slot _point unit Point.component (Point.AddPOI la lo) absurd
view _ = HH.div
             [css "container", style "margin-top:20px"]
             [HH.div
              [css "row"]
              [HH.div
               [css "col-md-12"]
               [HH.div
                [css "col-md-3 col-md-offset-1"]
                [HH.h2
                 []
                 [HH.text "ERROR Unknown page"]
                ]
               ]
              ]
             ]

-- |Converts login messages to root actions
loginMessageConv::Login.Message->Action
loginMessageConv (Login.SetUserMessage ui) = SetUserAction ui

-- | Handle the queries sent to the root page
handleQuery ∷ ∀ r o m a .
              MonadAff m ⇒ 
              MonadAsk r m ⇒
              ManageNavigation m =>
              Query a → H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  GotoPageRequest newpage a → do
    state ← H.get
    H.liftEffect $ log $ "GotoPageRequest to " <> show newpage
    decided <- pure $ maybe Login (const newpage) state.userInfo
    H.liftEffect $ log $ "GotoPageRequest was decided to be " <> show decided
    if decided /= newpage
      then do
        gotoPage Login
      else do
        H.put $ state { page = decided }
    pure (Just a)

-- | Handle the root containers actions
handleAction ∷ ∀ r o m . MonadAff m 
  => MonadAsk r m
  => ManageNavigation m
  => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction (SetUserAction ui) = do
    H.liftEffect $ log $ "Logged in user " <> show ui
    H.modify_ \st → st { userInfo = ui }

handleAction Logout = do
  state <- H.get
  H.liftEffect $ log "Trying to logout"
  case state.userInfo of
    Just (UserInfo ui) -> do
      H.liftEffect $ log "Logged out!"
      gotoPage Login
    Nothing -> do
      H.liftEffect $ log "Nothing to logout!"
  H.put state { userInfo = Nothing }
