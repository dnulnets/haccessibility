-- |
-- | The OpenLayers Intersecto Select module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Interaction.Select
  ( Select
  , SelectEvent
  , Key
  , create
  , onSelect
  , unSelect ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (
  Fn1
  , Fn3
  , runFn1
  , runFn3)

-- Effect imports
import Effect (Effect)

-- own imports
import OpenLayers.Feature as Feature

--
-- Foreign data types
-- 
foreign import data Select :: Type
foreign import data Key :: Type

type SelectEvent = {
  selected :: Array Feature.Feature       -- ^Array of selected features
  , deselected :: Array Feature.Feature   -- ^Array of deselected features
  , type :: String                -- ^Type of event
}

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (Nullable {|r}) (Effect Select)

create :: forall r . Maybe {|r} -> Effect Select
create opts = runFn1 createImpl (toNullable opts)

--
-- All on_ functions
--
foreign import onImpl :: forall v a. Fn3 String (v -> Effect a) Select (Effect Key)

onSelect :: (SelectEvent -> Effect Unit) -> Select -> Effect Key
onSelect fn self = runFn3 onImpl "select" fn self

--
-- All un_ functions
--
foreign import unImpl :: Fn3 String Key Select (Effect Unit)

unSelect :: Key -> Select -> Effect Unit
unSelect key self = runFn3 unImpl "select" key self
