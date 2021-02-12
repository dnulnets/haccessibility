-- |
-- | The endponit module, contains all endpoints that the app calls in the backend
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.Endpoint where

-- Language imports
import Prelude (class Show, ($), (<<<), (<>))

-- Data imports
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- Monad imports
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadAsk)

-- Routing imports
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex (RouteDuplex', optional, root, segment, string)

-- |The base URL for the api
newtype BaseURL = BaseURL String

-- |The show instance
instance showBaseURL :: Show BaseURL where
  show (BaseURL s) = "BaseURL " <> s

-- |The endpoint needed from the backend server
data Endpoint = Authenticate              -- ^The authenticate endpoint
                | Item (Maybe String)     -- ^The single item endpoint
                | Items                   -- ^The fetch items based on filter endpoint
                | ItemsAndValues          -- ^The fetch items based on filter endpoint
                | Attributes              -- ^Fetch all available attributes
                | Attribute String        -- ^Fetch all attributes on an item
                | UserProperties          -- ^User properties endpoint
                | Entities                -- ^Fetch entities on the IoT Hub absed on filter
                  { type    ::String       -- ^Type of entities
                    , attrs ::Maybe String -- ^What attributes to select in the response
                  }

-- |The interface problem
data Problem = Backend | NotAuthenticated
type Data a = Either Problem a

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- |Determine what backend it is that handles this endpoint
backend ::forall r m . MonadAsk { baseURL :: BaseURL, iothubURL::BaseURL | r } m
        => Endpoint -- ^The endpoint
        ->m BaseURL -- ^The URL to the backend that serves this endpoint
backend Authenticate = asks _.baseURL
backend (Item _) = asks _.baseURL
backend Items = asks _.baseURL
backend ItemsAndValues = asks _.baseURL
backend Attributes = asks _.baseURL
backend (Attribute _) = asks _.baseURL
backend (Entities _) = asks _.iothubURL
backend UserProperties = asks _.baseURL

-- |The endpoint codec
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Authenticate": "iothub" / "api" / "authenticate" / noArgs
  , "Items": "iothub" / "api" / "items" / noArgs
  , "ItemsAndValues": "iothub" / "api" / "itemsandvalues" / noArgs
  , "Attributes": "iothub" / "api" / "attributes" /noArgs
  , "UserProperties": "iothub" / "api" / "user" / "properties" /noArgs
  , "Attribute": "iothub" / "api" / "item" / string segment / "attributes"
  , "Item": "iothub" / "api" / "item" / (optional (string segment)) 
  , "Entities": "entities" ? { type : string, attrs : optional <<< string} }
