-- |
-- | The OpenLayers Overlay module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Overlay (Overlay, create) where

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
foreign import data Overlay :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Overlay))

create :: forall r . {|r} -> Effect (Maybe Overlay)
create o = toMaybe <$> runFn1 createImpl o
