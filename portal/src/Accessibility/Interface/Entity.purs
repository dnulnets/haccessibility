-- |
-- | The IoT Hub Entity interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.Entity where

-- Language imports
import Prelude

-- Data imports
import Data.Maybe (Maybe)
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))

-- Effects
import Effect.Aff.Class (class MonadAff)

-- Halogen imports
import Halogen (HalogenM, lift)

-- Our own imports
import Accessibility.Interface.Endpoint (Data)

--
-- Entity
--

-- |The date the entity was last observed
type DateObserved = { "type" :: String
                      , "value" :: {
                        "@type" :: String
                        , "@value":: String
                      }
                    }

-- |The location of the entity
type Location = { "type" :: String
                  , "value" :: {
                    "coordinates":: Array Number
                    , "type" :: String
                  }
                }

-- |The device connected to this entity
type RefDevice = {  "object" :: String
                    , "type" :: String
                  }

-- |The value associated with this entity
type Value = {  "type" :: String
                , "value" :: Number
              }

-- | Definition of the entity
newtype Entity = Entity { "id" :: String
                          , "type" :: String
                          , "dateObserved" :: DateObserved
                          , "location" :: Location
                          , "refDevice" :: RefDevice
                          , "temperature" :: Maybe Value
                          , "snowHeight" :: Maybe Value
                        }

instance decodeJsonEntity :: DecodeJson Entity where

    decodeJson json = do
      x <- decodeJson json
      _id <- x .: "id"
      _type <- x .: "type"
      _dateObserved <- x .: "dateObserved"
      _location <- x .: "location"
      _refDevice <- x .: "refDevice"
      _temperature <- x .:? "temperature"
      _snowHeight <- x .:? "snowHeight"
      pure $ Entity {id: _id, 
        "type": _type, 
        dateObserved: _dateObserved,
        location: _location,
        refDevice: _refDevice,
        temperature: _temperature,
        snowHeight: _snowHeight}

instance showEntity :: Show Entity where
  show (Entity e) = show e

-- |The class for Items management
class MonadAff m <= ManageEntity m where

  -- |Fetches a list of items based on the query parameters
  queryEntities :: String                   -- ^Type of entities
                -> m (Data (Array Entity)) -- ^List of Entities

-- |Avoid lift in the components
instance manageEntityHalogenM :: ManageEntity m => ManageEntity (HalogenM st act slots msg m) where
  queryEntities t = lift $ queryEntities t
