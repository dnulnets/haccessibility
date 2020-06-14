-- |
-- | The OpenLayers Tile module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Layer.Tile (
  Tile
  , RawTile

  , create) where

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

-- Import own modules
import OpenLayers.Layer.BaseTileLayer as BaseTileLayer

--
-- Foreign data types
-- 
foreign import data RawTile :: Type
type Tile = BaseTileLayer.BaseTileLayer RawTile

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 {|r} (Effect (Nullable Tile))

create :: forall r . {|r} -> Effect (Maybe Tile)
create o = toMaybe <$> runFn1 createImpl o
