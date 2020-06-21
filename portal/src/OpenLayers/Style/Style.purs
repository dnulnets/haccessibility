-- |
-- | The OpenLayers Style module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Style.Style (
  Style
  , setText
  , create
  , create') where

-- Standard import
import Prelude

-- Data imports
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)

-- Effect imports
import Effect (Effect)

-- Own imports
import OpenLayers.FFI as FFI
import OpenLayers.Style.Text as Text

--
-- Foreign data types
-- 
foreign import data Style :: Type

--
-- Function mapping
--
foreign import createImpl :: forall r . Fn1 (FFI.NullableOrUndefined {|r}) (Effect Style)

create :: forall r . Maybe {|r} -> Effect Style
create o = runFn1 createImpl (FFI.toNullable o)

create' :: Effect Style
create' = runFn1 createImpl FFI.undefined

--
-- Setters
--
foreign import setTextImpl :: Fn2 (Nullable Text.Text) (Style) (Effect Unit)
setText :: Maybe Text.Text -> Style -> Effect Unit
setText t self = runFn2 setTextImpl (toNullable t) self
