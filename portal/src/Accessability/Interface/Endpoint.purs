-- |
-- | The endponit module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Interface.Endpoint where

-- | Language imports
import Prelude

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex (RouteDuplex', path, root, segment, string, optional)

-- |The base URL for the api
newtype BaseURL = BaseURL String

-- |The show instance
instance showBaseURL :: Show BaseURL where
  show (BaseURL s) = s

-- |The nedpoint needed from the backend server
data Endpoint = Authenticate
                | Item (Maybe String)

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- |The endpoint codec
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Authenticate": path "api/authenticate" noArgs
  , "Item": path "api/item" (optional (string segment)) }
