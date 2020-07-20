-- |
-- | The GPS interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.GPS (class ManageGPS, getPosition) where

-- Language imports
import Prelude
import Control.Monad.Trans.Class (lift)

-- Halogen imports
import Halogen (HalogenM)

-- OpenLayers import
import OpenLayers.Geolocation as Geolocation

-- Heat imports
import Accessibility.Data.Route (Page)

-- |The class for authentication
class Monad m <= ManageGPS m where

  -- |Goto a page in the application
  getPosition :: Geolocation.Geolocation -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance gpsHalogenM :: ManageGPS m => ManageGPS (HalogenM st act slots msg m) where
  getPosition = lift <<< getPosition
