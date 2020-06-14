-- |
-- | The OpenLayers Vector module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.Vector (
  Vector
  , Style(..)
  , RawVector
  , StyleFunc

  , create
  
  , setStyle ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn1
  , Fn2
  , runFn1
  , runFn2)

-- Effect imports
import Effect (Effect)

-- Our own imports
import OpenLayers.Layer.BaseVectorLayer as BaseVectorLayer
import OpenLayers.Style.Style as Style
import OpenLayers.Feature as Feature

--
-- Our own data types
--
type StyleFunc = Feature.Feature->Number->Effect (Nullable Style.Style)
data Style =  Style Style.Style
            | StyleFunction StyleFunc
            | StyleArray (Array Style.Style)
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

--
-- Setters
--
foreign import setStyleImpl :: Fn2 Style.Style Vector (Effect Unit)
foreign import setStyleFImpl :: Fn2 StyleFunc Vector (Effect Unit)
foreign import setStyleAImpl :: Fn2 (Array Style.Style) Vector (Effect Unit)

setStyle::Style->Vector->Effect Unit
setStyle (Style s) self = runFn2 setStyleImpl s self
setStyle (StyleFunction f) self = runFn2 setStyleFImpl f self
setStyle (StyleArray a) self = runFn2 setStyleAImpl a self