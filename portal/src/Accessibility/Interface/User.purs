-- |
-- | The user interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.User where

-- Language imports
import Prelude

-- Data imports
import Data.DateTime.ISO (ISO)
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)

-- Halogen imports
import Halogen (HalogenM, lift)

-- Our own imports
import Accessibility.Interface.Endpoint (Data)
import Accessibility.Interface.Item (AttributeType)

-- | The enmueration for an operation
data Operation = EQ   -- Equal to
                | LT  -- Less than
                | LTE -- Less then or equal to
                | GT  -- Greater than
                | GTE -- Greater than
                | IN  -- In between range

operationToString:: Operation
                  -> String
operationToString EQ = "EQ"
operationToString LT = "LT"
operationToString LTE = "LTE"
operationToString GT = "GT"
operationToString GTE = "GTE"
operationToString IN = "IN"

operationToDisplay:: Operation
                  -> String
operationToDisplay EQ = "="
operationToDisplay LT = "<"
operationToDisplay LTE = "<="
operationToDisplay GT = ">"
operationToDisplay GTE = ">="
operationToDisplay IN = "between"

displayToOperation:: String
                  -> Maybe Operation
displayToOperation "=" = Just EQ
displayToOperation "<" = Just LT
displayToOperation "<=" = Just LTE
displayToOperation ">" = Just GT
displayToOperation ">=" = Just GTE
displayToOperation "between" = Just IN
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
      "EQ" -> Just EQ
      "LT" -> Just LT
      "LTE" -> Just LTE
      "GT" -> Just GT
      "GTE" -> Just GTE
      "IN" -> Just IN
      _ -> Nothing

-- |The AttributeValue
type UserProperty = { userPropertyId :: Maybe String -- User Property key
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
