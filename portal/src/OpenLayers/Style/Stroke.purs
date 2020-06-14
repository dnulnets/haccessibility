-- |
-- | The OpenLayers Stroke module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Style.Stroke (
  Stroke
  , create
  ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , runFn1)

-- Effect imports
import Effect (Effect)

--
-- Foreign data types
-- 
foreign import data Stroke :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Stroke))
create :: forall r . {|r} -> Effect (Maybe Stroke)
create o = toMaybe <$> runFn1 createImpl o
