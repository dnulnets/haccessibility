-- |
-- | The OpenLayers Projection module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Proj (
  SRS
  
  , epsg_3857
  , epsg_4326

  , fromLonLat
  , fromLonLat'
  , toLonLat
  , toLonLat' ) where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried
  ( Fn2
  , runFn2)

-- Our own imports
import OpenLayers.FFI as FFI

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
foreign import fromLonLatImpl :: Fn2 (Array Number) (FFI.NullableOrUndefined SRS) (Array Number)

fromLonLat :: Array Number->Maybe SRS-> Array Number
fromLonLat c s = runFn2 fromLonLatImpl c $ FFI.toNullable s

fromLonLat' :: Array Number -> Array Number
fromLonLat' c = runFn2 fromLonLatImpl c FFI.undefined

foreign import toLonLatImpl :: Fn2 (Array Number) (FFI.NullableOrUndefined SRS) (Array Number)

toLonLat :: Array Number->Maybe SRS-> Array Number
toLonLat c s = runFn2 toLonLatImpl c $ FFI.toNullable s

toLonLat' :: Array Number->Array Number
toLonLat' c = runFn2 toLonLatImpl c FFI.undefined
