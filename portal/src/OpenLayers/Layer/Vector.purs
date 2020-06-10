-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.Vector (
  Vector
  , RawVector
  
  ,create) where

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

-- Our own imports
import OpenLayers.Layer.BaseVectorLayer as BaseVectorLayer

--
-- Foreign data types
-- 
foreign import data RawVector :: Type
type Vector = BaseVectorLayer.BaseVectorLayer RawVector

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Vector))

create :: forall r . {|r} -> Effect (Maybe Vector)
create o = toMaybe <$> runFn1 createImpl o
