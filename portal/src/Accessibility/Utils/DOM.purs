-- |
-- | Some DOM functions that I need
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Utils.DOM (setInnerHTML) where

-- Standard library
import Prelude

-- Effect imports
import Effect (Effect)

-- |Import the DOM element type
import Web.DOM.Element (Element)

-- |Set inner HTML of an element.
foreign import setInnerHTML :: Element -> String -> Effect Unit
