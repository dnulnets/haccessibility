-- |
-- | The OpenLayers Control module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2020
-- |
module OpenLayers.Collection (
    Collection
    , RawCollection
    
    , create
    , extend) where

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

--
-- Foreign data types
-- 
foreign import data RawCollection :: Type->Type
type Collection a = RawCollection a

--
-- Function mapping
--
foreign import createImpl :: forall r t . Fn1 {|r} (Effect (Nullable (Collection t)))

create :: forall r t . {|r} -> Effect (Maybe (Collection t))
create o = toMaybe <$> runFn1 createImpl o

foreign import extendImpl :: forall t . Fn2 (Array t) (Collection t) (Collection t)

extend :: forall t . Array t -> Collection t -> Collection t
extend a c = runFn2 extendImpl a c
