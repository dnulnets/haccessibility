-- |
-- | The Result evaluation module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Util.Result (evaluateResult) where

-- Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

-- Halogen
import Halogen as H

-- Our own imports
import Accessibility.Interface.Endpoint (Data, Problem(..))

-- | Evaluates the result
evaluateResult :: forall d o m s a . o
            -> Data d
            -> H.HalogenM {alert::Maybe String|s} a () o m (Maybe d)
evaluateResult e dobj = do
  case dobj of
    (Left Backend) -> do
      H.modify_ $ _ {alert = Just "Server is not responding, try again later"}
      pure $ Nothing
    (Left NotAuthenticated) -> do
      H.modify_ $ _ {alert = Just "Authentication failed, please login again!"}
      H.raise e
      pure $ Nothing
    (Right attrs) -> do
      pure $ Just attrs
