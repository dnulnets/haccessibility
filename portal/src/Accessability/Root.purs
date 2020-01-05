-- |
-- | The Root container module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Root (component) where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))

import Control.Monad.Reader.Trans (class MonadAsk)

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

-- DOM import
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Web.HTML.Navigator.Geolocation (NavigatorGeolocation)

-- Our own stuff
import Accessability.Data.Route (Page(..))
import Accessability.Component.HTML.Utils (css,
                                  prop,
                                  href)
                                  
import Accessability.Component.Login as Login
import Accessability.Interface.Navigate (class ManageNavigation)
import Accessability.Interface.Authenticate (class ManageAuthentication)

type State = { enabled :: Boolean }

data Action = Toggle

-- | The set of slots for the root container
type ChildSlots = ( login ∷ Login.Slot Unit )

_login = SProxy::SProxy "login"
  
component ∷ ∀ r q i o m. MonadAff m
  => ManageAuthentication m
  => ManageNavigation m
  => MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
  => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState ∷ ∀ i. i → State
initialState _ = { enabled: false }

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
                      HH.li [css "nav-item active"] [HH.a [css "nav-link", href Home] [HH.text "Link 1"]],
                      HH.li [css "nav-item"] [HH.a [css "nav-link", href Home] [HH.text "Link 2"]],
                      HH.li [css "nav-item"] [HH.a [css "nav-link", href Home] [HH.text "Link 3"]]
                      ]          
                    ]

-- |The right navigation bar
navbarRight∷forall p . State -> HH.HTML p Action
navbarRight state = HH.a [css "navbar-text", href Home]
                      [HH.text "Not logged in"]

render ∷ ∀ r m . MonadAff m
  => ManageAuthentication m
  => ManageNavigation m
  => MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
  => State → H.ComponentHTML Action ChildSlots m
render state = HH.div [] [
  HH.header [] [navbar $ (navbarHeader "Accessability portal") <> [navbarLeft state, navbarRight state]],
  HH.main [css "container", HPA.role "main"][HH.slot _login unit Login.component unit absurd]]

handleAction ∷ ∀ r o m . MonadAff m 
  => MonadAsk { geo ∷ Maybe NavigatorGeolocation | r } m
  => Action → H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Toggle →
    H.modify_ \st → st { enabled = not st.enabled }
