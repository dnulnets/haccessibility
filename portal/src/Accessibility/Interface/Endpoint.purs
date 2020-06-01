-- |
-- | The endponit module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.Endpoint where

-- | Language imports
import Prelude (class Show, ($), (<<<))

import Data.Maybe (Maybe)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadAsk)

import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex (RouteDuplex', optional, root, segment, string)

-- |The base URL for the api
newtype BaseURL = BaseURL String

-- |The show instance
instance showBaseURL :: Show BaseURL where
  show (BaseURL s) = s

-- |The nedpoint needed from the backend server
data Endpoint = Authenticate
                | Item (Maybe String)
                | Items
                | Attributes
                | Attribute String
                | Entities { type::String, attrs::Maybe String }

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- |Determine what backend it is that handles this endpoint
backend::forall r m . MonadAsk { baseURL :: BaseURL, iothubURL::BaseURL | r } m => Endpoint->m BaseURL
backend Authenticate = asks _.baseURL
backend (Item _) = asks _.baseURL
backend Items = asks _.baseURL
backend Attributes = asks _.baseURL
backend (Attribute _) = asks _.baseURL
backend (Entities _) = asks _.iothubURL

-- |The endpoint codec
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Authenticate": "api" / "authenticate" / noArgs
  , "Items": "api" / "items" / noArgs
  , "Attributes": "api" / "attributes" /noArgs
  , "Attribute": "api" / "item" / string segment / "attributes"
  , "Item": "api" / "item" / (optional (string segment)) 
  , "Entities": "entities" ? { type : string, attrs : optional <<< string} }
