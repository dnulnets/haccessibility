-- |
-- | The user interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.User where

-- Language imports
import Prelude

-- Data imports
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)

-- Halogen imports
import Halogen (HalogenM, lift)

-- Our own imports
import Accessibility.Interface.Endpoint (Data)
import Accessibility.Interface.Item (AttributeType)

-- | The enumeration for the operation in the user property.
data Operation = OEQ   -- Equal to
                | OLT  -- Less than
                | OLTE -- Less then or equal to
                | OGT  -- Greater than
                | OGTE -- Greater than
                | OIN  -- In between range

operationToString:: Operation
                  -> String
operationToString OEQ = "EQ"
operationToString OLT = "LT"
operationToString OLTE = "LTE"
operationToString OGT = "GT"
operationToString OGTE = "GTE"
operationToString OIN = "IN"

operationToDisplay:: Operation
                  -> String
operationToDisplay OEQ = "="
operationToDisplay OLT = "<"
operationToDisplay OLTE = "<="
operationToDisplay OGT = ">"
operationToDisplay OGTE = ">="
operationToDisplay OIN = "between"

displayToOperation:: String
                  -> Maybe Operation
displayToOperation "=" = Just OEQ
displayToOperation "<" = Just OLT
displayToOperation "<=" = Just OLTE
displayToOperation ">" = Just OGT
displayToOperation ">=" = Just OGTE
displayToOperation "between" = Just OIN
displayToOperation _ = Nothing

instance showOperation :: Show Operation where
  show = operationToString

instance encodeJsonOperation :: EncodeJson Operation where
  encodeJson is = encodeJson $ operationToString is

instance decodeJsonOperation :: DecodeJson Operation where
  decodeJson json = do
    string <- decodeJson json
    let
      decodeError = "Could not decode Operation from " <> string
    note decodeError (fromString string)
    where
    fromString = case _ of
      "EQ" -> Just OEQ
      "LT" -> Just OLT
      "LTE" -> Just OLTE
      "GT" -> Just OGT
      "GTE" -> Just OGTE
      "IN" -> Just OIN
      _ -> Nothing

-- |The AttributeValue
type UserProperty = { userPropertyId :: Maybe String -- | User Property key
                    , attributeId    :: Maybe String      -- ^The key to the attribute
                    , name           :: String            -- ^The name of the attribute
                    , group          :: String            -- ^The group the attribute belongs to
                    , displayName    :: String            -- ^The display name of the attribute
                    , description    :: String            -- ^The description of the attribute 
                    , typeof         :: AttributeType     -- ^The type of the attribute
                    , unit           :: String            -- ^The unit of the attribute
                    , value          :: Maybe String      -- ^The value of the attribute
                    , value1         :: Maybe String      -- ^The value of the attribute
                    , operation      :: Maybe Operation
                    , negate         :: Maybe Boolean }

-- |The attribute change order
type UserPropertyChange = { userPropertyId :: Maybe String   -- ^The key to the record
                          , attributeId  :: Maybe String
                          , operation    :: Maybe Operation
                          , negate       :: Maybe Boolean
                          , value          :: Maybe String   -- ^The value
                          , value1         :: Maybe String   -- ^The value
                        }

-- |The class for Items management
class Monad m <= ManageUser m where

  -- |Update the attributes for an item
  updateUserProperties  :: Array UserPropertyChange   -- ^The array of change orders
                        -> m (Maybe Unit)         -- ^Nothing to return

  -- |Fetches all attributes and values for an item
  queryUserProperties :: m (Data (Array UserProperty))    -- ^List of attributes and values for a specific item

-- |Avoid lift in the components
instance manageItemHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  updateUserProperties a = lift $ updateUserProperties a
  queryUserProperties = lift queryUserProperties
