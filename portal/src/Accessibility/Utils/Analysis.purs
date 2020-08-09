-- |
-- | The Analysis module
-- |
-- | Written by Tomas Stenlund, Sundsvall,Sweden (c) 2020
-- |
module Accessibility.Utils.Analysis where

-- Language imports

import Prelude

import Accessibility.Interface.Item (AttributeType(..), AttributeValue)
import Accessibility.Interface.User (Operation(..), UserProperty)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr)
import Global (readFloat)

-- |The items value from a users perspective
newtype ItemValue = ItemValue { positive   :: Int -- Number of positive properties
                              , negative  :: Int  -- Number of negative properties
                              , unknown   :: Int  -- Number of unknown properties
                              , positiveAttributes :: Array UserProperty  -- List of positive properties
                              , negativeAttributes :: Array UserProperty  -- List of negative properties
                              , unknownAttributes  :: Array UserProperty  -- List of unknown properties
                              }

instance showItemValue :: Show ItemValue where
  show (ItemValue r) = "ItemValue " <> show r

instance monoidItemValue :: Monoid ItemValue where
  mempty = ItemValue {positive:0, negative:0, unknown:0
                      , positiveAttributes: []
                      , negativeAttributes: []
                      , unknownAttributes: []}

instance semigroupItemValue :: Semigroup ItemValue where
  append (ItemValue i1) (ItemValue i2) = ItemValue {positive: i1.positive+i2.positive
                                                    , negative: i1.negative+i2.negative
                                                    , unknown: i1.unknown+i2.unknown
                                                    , positiveAttributes: i1.positiveAttributes <> i2.positiveAttributes
                                                    , negativeAttributes: i1.negativeAttributes <> i2.negativeAttributes
                                                    , unknownAttributes: i1.unknownAttributes <> i2.unknownAttributes}

-- |Determines the value of a POI based on the users properties
evaluatePOI::Array UserProperty -> Array AttributeValue -> ItemValue
evaluatePOI aup aav = foldr append mempty ((evaluateUserProperty $ toAttributeValueMap aav) <$> aup )

  where
    
    toAttributeValueMap::Array AttributeValue -> Map.Map String AttributeValue
    toAttributeValueMap a = Map.fromFoldable $ (\av->Tuple (fromMaybe "" av.attributeId) av) <$> a

    evaluateUserProperty::Map.Map String AttributeValue -> UserProperty -> ItemValue
    evaluateUserProperty msa up = case join $ Map.lookup <$> up.attributeId <*> Just msa of
                      Nothing -> ItemValue {positive: 0
                                          , negative: 0
                                          , unknown: 1
                                          , positiveAttributes: []
                                          , negativeAttributes: []
                                          , unknownAttributes: [up]}
                      Just a -> if (evaluate up a)
                                  then ItemValue {positive: 1, negative: 0, unknown: 0
                                                , positiveAttributes: [up]
                                                , negativeAttributes: []
                                                , unknownAttributes: []}
                                  else ItemValue {positive: 0, negative: 1, unknown: 0
                                                , positiveAttributes: []
                                                , negativeAttributes: [up]
                                                , unknownAttributes: []}

    evaluate::UserProperty->AttributeValue->Boolean
    evaluate up av = fromMaybe false (notit <$> up.negate 
                                            <*> (operate  <$> up.operation 
                                                          <*> av.value 
                                                          <*> up.value 
                                                          <*> (Just up.value1) 
                                                          <*> (Just av.typeof)))

    -- Logical xor
    notit::Boolean->Boolean->Boolean
    notit true v = not v
    notit false v = v

    -- Perform the operation for the user property
    operate::Operation->String->String->Maybe String->AttributeType->Boolean
    operate OEQ v1 v2 _ TextType = v1 == v2
    operate OEQ v1 v2 _ BooleanType = v1 == v2
    operate OEQ v1 v2 _ NumberType = (readFloat v1) == (readFloat v2)
    operate OLT v1 v2 _ TextType = v1 < v2
    operate OLT v1 v2 _ BooleanType = false
    operate OLT v1 v2 _ NumberType = (readFloat v1) < (readFloat v2)
    operate OLTE v1 v2 _ TextType = v1 <= v2
    operate OLTE v1 v2 _ BooleanType = false
    operate OLTE v1 v2 _ NumberType = (readFloat v1) <= (readFloat v2)
    operate OGT v1 v2 _ TextType = v1 > v2
    operate OGT v1 v2 _ BooleanType = false
    operate OGT v1 v2 _ NumberType = (readFloat v1) > (readFloat v2)
    operate OGTE v1 v2 _ TextType = v1 >= v2
    operate OGTE v1 v2 _ BooleanType = false
    operate OGTE v1 v2 _ NumberType = (readFloat v1) >= (readFloat v2)
    operate OIN v1 v21 (Just v22) TextType = v1 >= v21 && v1 <= v22
    operate OIN v1 v21 (Just v22) BooleanType = false
    operate OIN v1 v21 (Just v22) NumberType = (readFloat v1) > (readFloat v21) && (readFloat v1) < (readFloat v22)
    operate OIN _ _ Nothing _ = false
