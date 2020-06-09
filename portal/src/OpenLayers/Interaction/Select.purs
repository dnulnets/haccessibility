-- |
-- | The OpenLayers module
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
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , Fn3
  , Fn4
  , Fn5
  , runFn1
  , runFn2
  , runFn3
  , runFn4
  , runFn5)

-- Effect imports
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

-- own imports
import OpenLayers.Feature

--
-- Foreign data types
-- 
foreign import data Select :: Type
foreign import data Key :: Type

type SelectEvent = {
  selected :: Array Feature       -- ^Array of selected features
  , deselected :: Array Feature   -- ^Array of deselected features
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
foreign import unImpl :: forall v a. Fn3 String Key Select (Effect Unit)

unSelect :: forall a v. Key -> Select -> Effect Unit
unSelect key self = runFn3 unImpl "select" key self
