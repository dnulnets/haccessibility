-- |
-- | The navigation interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Interface.Navigate (class ManageNavigation, gotoPage) where

-- Language imports
import Prelude

import Control.Monad.Trans.Class (lift)

-- Halogen imports
import Halogen (HalogenM)

-- Heat imports
import Accessability.Data.Route (Page)
            
-- |The class for authentication
class Monad m ⇐ ManageNavigation m where

  -- |Goto a page in the application
  gotoPage :: Page -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance gotoPageHalogenM :: ManageNavigation m => ManageNavigation (HalogenM st act slots msg m) where
  gotoPage = lift <<< gotoPage
