-- |
-- | The items interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Accessability.Interface.Item where

-- Language imports
import Prelude

import Data.DateTime.ISO (ISO)
import Data.Maybe (Maybe(..))
import Data.Either (note)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)

-- Halogen imports
import Halogen (HalogenM, lift)

-- | The enmueration for the source of the item
data ItemSource = Human | Machine

itemSourceToString::ItemSource->String
itemSourceToString Human = "Human"
itemSourceToString Machine = "Machine"

instance showItemSource :: Show ItemSource where
  show = itemSourceToString

instance encodeJsonItemSource :: EncodeJson ItemSource where
  encodeJson is = encodeJson $ itemSourceToString is

instance decodeJsonItemSource :: DecodeJson ItemSource where
  decodeJson json = do
    string <- decodeJson json
    let decodeError = "Could not decode ItemSource from " <> string
    note decodeError (fromString string)
    where
        fromString = case _ of
            "Human" -> Just Human
            "Machine" -> Just Machine
            _ -> Nothing

-- | The enumeration for the modifier of the item
data ItemModifier = Static | Transient

itemModifierToString::ItemModifier->String
itemModifierToString Static = "Static"
itemModifierToString Transient = "Transient"

instance showItemModifier :: Show ItemModifier where
  show = itemModifierToString

instance encodeJsonItemModifier :: EncodeJson ItemModifier where
  encodeJson im = encodeJson $ itemModifierToString im

instance decodeJsonItemModifier :: DecodeJson ItemModifier where
  decodeJson json = do
    string <- decodeJson json
    let decodeError = "Could not decode ItemModifier from " <> string
    note decodeError (fromString string)
    where
        fromString = case _ of
            "Static" -> Just Static
            "Transient" -> Just Transient
            _ -> Nothing

-- | The enumeration for the approval of the item
data ItemApproval = Waiting | Approved | Denied

itemApprovalToString::ItemApproval->String
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
    let decodeError = "Could not decode ItemApproval from " <> string
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
type Item = {
    id            :: Maybe String          -- ^ Item key
    , name        :: String            -- ^ The short name of the item
    , guid        :: String            -- ^ The external unique identifier of the item
    , created     :: ISO     -- ^ The creation time
    , description :: String    -- ^ The description of the item
    , source      :: ItemSource   -- ^ How the items online state is determined
    , modifier    :: ItemModifier       -- ^ The type of the item
    , approval    :: ItemApproval   -- ^ The tstae of approval for the item
    , latitude    :: Number      -- ^ The latitude of the item
    , longitude   :: Number     -- ^ The longitude of the item
    , distance    :: Maybe Number -- ^ The distance to a specified point at the time of the query
    }

-- | The argument for the queryitems query
type QueryItems = {
    longitude  ::  Maybe Number -- ^ The longitude of search circle
    , latitude :: Maybe Number  -- ^ The latitude of the search circle
    , distance :: Maybe Number  -- ^ The distance or size of the search circle
    , limit    :: Maybe Int -- ^ Max number of items
    , text     :: Maybe String -- ^ The text that must be present in name or description
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
class Monad m ⇐ ManageItem m where

  -- |Fetches a list of items based on the query parameters
  queryItems∷QueryItems     -- ^Query parameters
    →m (Maybe (Array Item))  -- ^List of items
  
-- |Avoid lift in the components
instance manageItemHalogenM :: ManageItem m => ManageItem (HalogenM st act slots msg m) where
  queryItems = lift <<< queryItems
