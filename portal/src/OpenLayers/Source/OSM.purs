-- |
-- | The OpenLayers Openstreetmap API module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Source.OSM (OSM, create) where

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
foreign import data OSM :: Type

instance showOSM :: Show OSM where
  show _ = "OSM"
--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable OSM))

create :: forall r . {|r} -> Effect (Maybe OSM)
create o = toMaybe <$> runFn1 createImpl o
