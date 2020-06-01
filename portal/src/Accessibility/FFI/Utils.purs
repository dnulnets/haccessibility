-- |
-- | The FFI Utility module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module Accessibility.FFI.Utils (enableTooltips) where

-- Standard prelude
import Prelude

-- Data modules
import Data.Function.Uncurried (Fn0, runFn0)

-- Effect modules
import Effect (Effect)
--
-- Enable tooltips
--
foreign import enableTooltipsImpl :: Fn0 (Effect Unit)

-- |Enables the tooltips function in Bootstrap4
enableTooltips::Effect Unit
enableTooltips = runFn0 enableTooltipsImpl