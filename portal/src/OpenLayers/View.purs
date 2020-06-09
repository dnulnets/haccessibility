-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.View (View, create, getProjection) where

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

--
-- Foreign data types
-- 
foreign import data View :: Type
instance showView :: Show View where
  show _ = "View"
--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable View))

create :: forall r . {|r} -> Effect (Maybe View)
create o = toMaybe <$> runFn1 createImpl o

--
-- get functins
--
foreign import getProjectionImpl :: Fn1 View (Effect (Nullable String))

getProjection :: View -> Effect (Maybe String)
getProjection self = toMaybe <$> runFn1 getProjectionImpl self
