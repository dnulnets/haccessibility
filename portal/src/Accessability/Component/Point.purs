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
import Accessability.Interface.Item (class ManageItem, queryItems, Item)
import Accessability.Interface.Navigate (class ManageNavigation)
import Control.Alt ((<|>))
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Array ((!!))
import Data.Foldable (sequence_)
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
type Slot p = ∀ q . H.Slot q Void p

-- | State for the component
type State = { alert::Maybe String }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i   -- ^ Initial input
  → State               -- ^ The state
initialState _ = { alert : Nothing }

-- | Internal form actions
data Action = Initialize
            | Finalize
            | Submit Event
            | Input
  
-- | The component definition
component ∷ ∀ r q i o m . MonadAff m
            ⇒ ManageNavigation m
            => MonadAsk r m
            => ManageEntity m
            => ManageItem m
            ⇒ H.Component HH.HTML q i o m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
      initialize = Just Initialize,
      finalize = Just Finalize
     }
    }

nearbyAlert::forall p i . Maybe String -> HH.HTML p i
nearbyAlert (Just t) = HH.div [css "alert alert-danger"] [HH.text $ t]
nearbyAlert Nothing = HH.div [] []

-- | Render the nearby page
render ∷ ∀ m . MonadAff m ⇒ State -- ^ The state to render
  → H.ComponentHTML Action () m   -- ^ The components HTML
render state = HH.div
               [css "container-fluid"]
               [HH.div [css "row"] [HH.div [css "col-xs-12 col-md-12"] [
                   nearbyAlert state.alert]
                ],
                HH.form [css "form-signin", HE.onSubmit (Just <<< Submit)]
                   [HH.h1 [css "mt-3"] [HH.text "POI Information"],
                    HH.div [css "form-group"] [
                        HH.label [HP.for "name"] [HH.text "Name"],
                        HH.input [css "form-control", HP.id_ "name", HP.type_ HP.InputText,
                                  HPA.label "name", HP.placeholder "name",
                                  HE.onValueChange \v -> Just $ Input]
                    ],
                    HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonSubmit] [HH.text "Save"]
                   ]
                ]

-- | Handles all actions for the login component
handleAction ∷ ∀ r o m . MonadAff m
            ⇒ ManageNavigation m
            => ManageEntity m
            => ManageItem m
            => MonadAsk r m
  ⇒ Action -- ^ The action to handle
  → H.HalogenM State Action () o m Unit -- ^ The handled action

-- | Initialize action
handleAction Initialize = do
    H.liftEffect $ log "Initialize Nearby Component"

-- | Finalize action
handleAction Finalize = do
    H.liftEffect $ log "Finalize Nearby Component"

handleAction (Submit event) = do
    H.liftEffect $ Event.preventDefault event
    H.liftEffect $ log "Form submitted"

handleAction Input = do
    H.liftEffect $ log "Input changed"
