-- |
-- | The endponit module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Interface.Endpoint where

-- | Language imports
import Prelude hiding ((/))

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
--import Routing.Duplex.Generic.Syntax
--import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic
import Routing.Duplex.Generic.Syntax
--import Routing.Duplex (RouteDuplex', path, root, param, segment, string, optional, (?))
import Routing.Duplex

-- |The base URL for the api
newtype BaseURL = BaseURL String

-- |The show instance
instance showBaseURL :: Show BaseURL where
  show (BaseURL s) = s

-- |The nedpoint needed from the backend server
data Endpoint = Authenticate
                | Item (Maybe String)
                | Items
                | IOTHUBEntities { type::String, attrs::Maybe String }

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- |The endpoint codec
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Authenticate": "api" / "authenticate" / noArgs
  , "Items": "api" / "items" / noArgs
  , "Item": "api" / "item" / (optional (string segment)) 
  , "IOTHUBEntities": "entities" ? { type : string, attrs : optional <<< string} }
