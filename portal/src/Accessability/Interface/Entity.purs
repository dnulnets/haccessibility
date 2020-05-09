-- |
-- | The IoT Hub Entity interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Interface.Entity where

-- Language imports
import Prelude

import Data.DateTime.ISO (ISO, unwrapISO)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Argonaut (class DecodeJson,
                      class EncodeJson,
                      decodeJson, encodeJson, jsonEmptyObject,
                      (.:),
                      (:=),
                      (~>))

-- Halogen imports
import Halogen (HalogenM, lift)

--
-- Entity
--

-- | Definition of the entity
type Entity = {
    "dateObserved" :: {
        "type" :: String,
        "value" :: {
            "@type" :: String,
            "@value":: String
        }
    },
    "id" :: String,
    "location" :: {
        "type" :: String,
        "value" :: {
            "coordinates":: Array Number,
            "type" :: String
        }
    },
    "refDevice" :: {
        "object" :: String,
        "type" :: String
    },
    "temperature" :: {
        "type" :: String,
        "value" :: Number
    },
    "type" :: String
  }

-- |The class for Items management
class Monad m ⇐ ManageEntity m where

  -- |Fetches a list of items based on the query parameters
  queryEntities::String             -- ^Type of entities
    -> Maybe String                       -- ^Type of attributes to return
    -> m (Maybe (Array Entity))     -- ^List of Entities
  
-- |Avoid lift in the components
instance manageEntityHalogenM :: ManageEntity m => ManageEntity (HalogenM st act slots msg m) where
  queryEntities t a = lift $ queryEntities t a
