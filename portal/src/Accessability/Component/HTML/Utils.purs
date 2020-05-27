-- |
-- | The HTML Utilities module
-- |
-- | Written by Tomas Stenlund (c), 2019
-- |
module Accessability.Component.HTML.Utils
    ( css
    , style
    , prop
    , href
    , maybeElem
    , maybeOrElem
    , maybeElem_
    , maybeOrElem_
    , whenElem
    ) where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..))

-- Routing imports
import Routing.Duplex (print)

-- Halogen imports
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (AttrName(..), PropName(..))

-- Our own import
import Accessability.Data.Route (Page, routeCodec)

-- | Helper function for adding class to HTML tags
css ∷ forall r i. String → HH.IProp ( class ∷ String | r ) i
css = HP.class_ <<< HH.ClassName

-- | A helper for the style property
style ∷ forall r i. String → HH.IProp (style ∷ String | r) i
style = HP.prop (PropName "style")

-- | A generic property string function
prop ∷ forall r i. String → String → HP.IProp r i
prop name = HP.attr (AttrName name)

-- |A href that is built on the route data type instead of a string
href :: forall r i. Page -> HH.IProp ( href :: String | r) i
href = HP.href <<< append "#" <<< print routeCodec

-- | Render a fragment if the the value exists (Just), empty if not (Nothing)
maybeElem ∷ forall p i a. Maybe a → (a → HH.HTML p i) → Array (HH.HTML p i)
maybeElem (Just x) f = [ f x ]
maybeElem _ _ = []

-- | Render a fragment if the the value exists (Just), empty if not (Nothing)
maybeElem_ ∷ forall p i a. Maybe a → HH.HTML p i → Array (HH.HTML p i)
maybeElem_ (Just _) f = [ f ]
maybeElem_ _ _ = []

-- | Render a fragment if the the value exists (Just), and another for if it does not exist (Nothing)
maybeOrElem ∷ forall p i a. Maybe a → HH.HTML p i → (a → HH.HTML p i) → Array (HH.HTML p i)
maybeOrElem (Just x) _ f = [ f x ]
maybeOrElem _ o _ = [ o ]

-- | Render a fragment if the the value exists (Just), and another for if it does not exist (Nothing)
maybeOrElem_ ∷ forall p i a. Maybe a → HH.HTML p i → HH.HTML p i → Array (HH.HTML p i)
maybeOrElem_ (Just _) _ f = [ f ]
maybeOrElem_ _ o _ = [ o ]

-- | Render a fragment if the value is true, empty if not
whenElem ∷ forall p i. Boolean → (Unit → HH.HTML p i) → HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""
