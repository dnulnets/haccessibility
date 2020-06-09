-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Proj (
  SRS
  
  , epsg_3857
  , epsg_4326

  , fromLonLat
  , toLonLat ) where

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

--
-- Own datatypes
--
newtype SRS = SRS String
foreign import epsg_3857::SRS
foreign import epsg_4326::SRS

--
-- Function mapping
--
foreign import fromLonLatImpl :: Fn2 (Array Number) (Nullable SRS) (Array Number)

fromLonLat :: Array Number->Maybe SRS-> Array Number
fromLonLat c s = runFn2 fromLonLatImpl c $ toNullable s

foreign import toLonLatImpl :: Fn2 (Array Number) (Nullable SRS) (Array Number)

toLonLat :: Array Number->Maybe SRS-> Array Number
toLonLat c s = runFn2 toLonLatImpl c $ toNullable s