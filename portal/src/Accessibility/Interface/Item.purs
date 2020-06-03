-- |
-- | The items interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessibility.Interface.Item where

-- Language imports
import Prelude

-- Data imports
import Data.DateTime.ISO (ISO)
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)

-- Halogen imports
import Halogen (HalogenM, lift)

-- | The enmueration for the source of the item
data ItemSource = Human   -- ^Human has entered the item
                | Machine -- ^Machine has enterd the item

itemSourceToString:: ItemSource
                  -> String
itemSourceToString Human = "Human"

itemSourceToString Machine = "Machine"

instance showItemSource :: Show ItemSource where
  show = itemSourceToString

instance encodeJsonItemSource :: EncodeJson ItemSource where
  encodeJson is = encodeJson $ itemSourceToString is

instance decodeJsonItemSource :: DecodeJson ItemSource where
  decodeJson json = do
    string <- decodeJson json
    let
      decodeError = "Could not decode ItemSource from " <> string
    note decodeError (fromString string)
    where
    fromString = case _ of
      "Human" -> Just Human
      "Machine" -> Just Machine
      _ -> Nothing

-- | The enumeration for the modifier of the item
data ItemModifier = Static      -- ^This is a static item
                  | Transient   -- ^This is a dynamic item that vanishes with time

itemModifierToString :: ItemModifier -> String
itemModifierToString Static = "Static"

itemModifierToString Transient = "Transient"

instance showItemModifier :: Show ItemModifier where
  show = itemModifierToString

instance encodeJsonItemModifier :: EncodeJson ItemModifier where
  encodeJson im = encodeJson $ itemModifierToString im

instance decodeJsonItemModifier :: DecodeJson ItemModifier where
  decodeJson json = do
    string <- decodeJson json
    let
      decodeError = "Could not decode ItemModifier from " <> string
    note decodeError (fromString string)
    where
    fromString = case _ of
      "Static" -> Just Static
      "Transient" -> Just Transient
      _ -> Nothing

-- | The enumeration for the approval of the item
data ItemApproval = Waiting   -- ^The approval has not yet been approved
                  | Approved  -- ^The item is vetted and approved
                  | Denied    -- ^The item has been denied

itemApprovalToString :: ItemApproval -> String
itemApprovalToString Waiting = "Waiting"

itemApprovalToString Approved = "Approved"

itemApprovalToString Denied = "Denied"

instance showItemApproval :: Show ItemApproval where
  show = itemApprovalToString

instance encodeJsonItemApproval :: EncodeJson ItemApproval where
  encodeJson il = encodeJson $ itemApprovalToString il

instance decodeJsonItemApproval :: DecodeJson ItemApproval where
  decodeJson json = do
    string <- decodeJson json
    let
      decodeError = "Could not decode ItemApproval from " <> string
    note decodeError (fromString string)
    where
    fromString = case _ of
      "Waiting" -> Just Waiting
      "Approved" -> Just Approved
      "Denied" -> Just Denied
      _ -> Nothing

--
-- Item
--
-- | Definition of the item
type Item = { id            :: Maybe String -- ^ Item key
              , name        :: String       -- ^ The short name of the item
              , guid        :: String       -- ^ The external unique identifier of the item
              , created     :: ISO          -- ^ The creation time
              , description :: String       -- ^ The description of the item
              , source      :: ItemSource   -- ^ How the items online state is determined
              , modifier    :: ItemModifier -- ^ The type of the item
              , approval    :: ItemApproval -- ^ The tstae of approval for the item
              , latitude    :: Number       -- ^ The latitude of the item
              , longitude   :: Number       -- ^ The longitude of the item
              , distance    :: Maybe Number -- ^ The distance to a specified point at the time of the query
            }

--
-- AttributeValue
--

-- | The enumeration for the attribute type
data AttributeType  = TextType    -- ^The attribute is of type text
                    | NumberType  -- ^The attribute is of type number
                    | BooleanType -- ^The attribute is of type boolean

attributeTypeToString :: AttributeType -> String
attributeTypeToString TextType = "TextType"
attributeTypeToString NumberType = "NumberType"
attributeTypeToString BooleanType = "BooleanType"

instance showAttributeType :: Show AttributeType where
  show = attributeTypeToString

instance encodeJsonAttributeType :: EncodeJson AttributeType where
  encodeJson il = encodeJson $ attributeTypeToString il

instance decodeJsonAttributeType :: DecodeJson AttributeType where
  decodeJson json = do
    string <- decodeJson json
    let
      decodeError = "Could not decode AttributeType from " <> string
    note decodeError (fromString string)
    where
    fromString = case _ of
      "NumberType" -> Just NumberType
      "BooleanType" -> Just BooleanType
      "TextType" -> Just TextType
      _ -> Nothing

-- |The AttributeValue
type AttributeValue = { attributeId   :: Maybe String      -- ^Attribute key
                        , name        :: String            -- ^The name of the attribute
                        , description :: String            -- ^The description of the attribute 
                        , typeof      :: AttributeType     -- ^The type of the attribute
                        , unit        :: String            -- ^The unit of the attribute
                        , value       :: Maybe String      -- ^The value of the attribute
                        , itemId      :: Maybe String      -- ^The key to the item that the AttributeValue belongs to
                        , attributeValueId :: Maybe String -- ^The key to the AtributeValue record    
                      }

-- |The attribute change order
type AttributeChange = {  attributeValueId      :: Maybe String   -- ^The key to the record
                          , value               :: Maybe String   -- ^The value
                          , attributeId         :: Maybe String   -- ^The key to the attribute
                        }

-- | The argument for the queryitems query
type QueryItems = { longitude   :: Maybe Number -- ^ The longitude of search circle
                    , latitude  :: Maybe Number -- ^ The latitude of the search circle
                    , distance  :: Maybe Number -- ^ The distance or size of the search circle
                    , limit     :: Maybe Int    -- ^ Max number of items
                    , text      :: Maybe String -- ^ The text that must be present in name or description
                  }

--
--instance encodeJsonQueryItems :: EncodeJson QueryItems where
--  encodeJson b
--    = "longitude" := b.queryItemsLongitude
--    ~> "latitude" := b.queryItemsLatitude
--    ~> "distance" := b.queryItemsDistance
--    ~> "limit" := b.queryItemsLimit
--    ~> "String" := b.queryItemsText
--    ~> jsonEmptyObject
--
-- |The class for Items management
class Monad m <= ManageItem m where

  -- |Add an item to the backend
  addItem :: Item             -- ^The item to be added, id is ignored
          -> m (Maybe Item)   -- ^The item as stored in the backend with new id

  updateItem  :: Item             -- ^The item to be updated
              -> m (Maybe Item)   -- ^The item as stored in the backend

  -- |Fetches a list of items based on the query parameters
  queryItem :: String         -- ^The key
            -> m (Maybe Item) -- ^List of items

  -- |Fetches a list of items based on the query parameters
  queryItems  :: QueryItems             -- ^The query
              -> m (Maybe (Array Item)) -- ^List of items

  -- |Fetches all attributes
  queryAttributes :: m (Maybe (Array AttributeValue)) -- ^List of attributes

  -- |Update the attributes for an item
  updateItemAttributes  :: String                 -- ^The key to the item
                        -> Array AttributeChange  -- ^The array of change orders
                        -> m (Maybe Unit)         -- ^Nothing to return

  -- |Fetches all attributes and values for an item
  queryItemAttributes :: String                           -- ^The key to the item
                      -> m (Maybe (Array AttributeValue)) -- ^List of attributes and values for a specific item

-- |Avoid lift in the components
instance manageItemHalogenM :: ManageItem m => ManageItem (HalogenM st act slots msg m) where
  addItem = lift <<< addItem
  updateItem = lift <<< updateItem
  queryItem = lift <<< queryItem
  queryItems = lift <<< queryItems
  queryAttributes = lift queryAttributes
  updateItemAttributes k a = lift $ updateItemAttributes k a
  queryItemAttributes = lift <<< queryItemAttributes
