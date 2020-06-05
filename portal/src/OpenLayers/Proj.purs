-- |
-- | The OpenLayers Feature module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Proj (Code(..), fromLonLat) where

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

data Code = EPSG_3857 | EPSG_4326

-- |Code to string interface
instance showCode :: Show Code where
  show EPSG_3857 = "EPSG:3857"
  show EPSG_4326 = "EPSG:4326"

--
-- Function mapping
--
foreign import fromLonLatImpl :: Fn2 (Array Number) (Nullable String) (Array Number)

fromLonLat :: Array Number->Maybe Code-> Array Number
fromLonLat c s = runFn2 fromLonLatImpl c $ toNullable (show <$> s)

foreign import toLonLatImpl :: Fn2 (Array Number) (Nullable String) (Array Number)

toLonLat :: Array Number->Maybe Code-> Array Number
toLonLat c s = runFn2 toLonLatImpl c $ toNullable (show <$> s)