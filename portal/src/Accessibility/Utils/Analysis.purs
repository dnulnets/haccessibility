-- |
-- | The Analysis module
-- |
-- | Written by Tomas Stenlund, Sundsvall,Sweden (c) 2020
-- |
module Accessibility.Utils.Analysis where

-- Language imports

import Prelude

import Accessibility.Interface.Item (AttributeType(..), AttributeValue, ItemValue(..))
import Accessibility.Interface.User (Operation(..), UserProperty)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr)
import Global (readFloat)

-- |Determines the value of a POI based on the users properties
evaluatePOI::Array UserProperty -> Array AttributeValue -> ItemValue
evaluatePOI aup aav = foldr append mempty ((evaluateUserProperty $ toAttributeValueMap aav) <$> aup )

  where
    
    toAttributeValueMap::Array AttributeValue -> Map.Map String AttributeValue
    toAttributeValueMap a = Map.fromFoldable $ (\av->Tuple (fromMaybe "" av.attributeId) av) <$> a

    evaluateUserProperty::Map.Map String AttributeValue -> UserProperty -> ItemValue
    evaluateUserProperty msa up = case join $ Map.lookup <$> up.attributeId <*> Just msa of
                      Nothing -> ItemValue {positive: 0, negative: 0, unknown: 1}
                      Just a -> if (evaluate up a)
                                  then ItemValue {positive: 1, negative: 0, unknown: 0}
                                  else ItemValue {positive: 0, negative: 1, unknown: 0}

    evaluate::UserProperty->AttributeValue->Boolean
    evaluate up av = case up.operation of
                        Nothing -> false
                        (Just o) -> case o of 
                          OEQ -> fromMaybe false (notit <$> up.negate <*> (isEQ <$> av.value <*> up.value <*> (Just av.typeof)))
                          OLT -> fromMaybe false (notit <$> up.negate <*> (isLT <$> av.value <*> up.value <*> (Just av.typeof)))
                          OLTE -> fromMaybe false (notit <$> up.negate <*> (isLTE <$> av.value <*> up.value <*> (Just av.typeof)))
                          OGT -> fromMaybe false (notit <$> up.negate <*> (isGT <$> av.value <*> up.value <*> (Just av.typeof)))
                          OGTE -> fromMaybe false (notit <$> up.negate <*> (isGTE <$> av.value <*> up.value <*> (Just av.typeof)))
                          OIN -> fromMaybe false (notit <$> up.negate <*> (isIN <$> av.value <*> up.value <*> up.value1 <*> (Just av.typeof)))

    -- Logical xor
    notit::Boolean->Boolean->Boolean
    notit true v = not v
    notit false v = v

    isEQ::String->String->AttributeType->Boolean
    isEQ v1 v2 t = case t of
      TextType -> v1 == v2
      BooleanType -> v1 == v2
      NumberType -> (readFloat v1) == (readFloat v2)

    isLT::String->String->AttributeType->Boolean
    isLT v1 v2 t = case t of
      TextType -> v1 < v2
      BooleanType -> false
      NumberType -> (readFloat v1) < (readFloat v2)

    isLTE::String->String->AttributeType->Boolean
    isLTE v1 v2 t = case t of
      TextType -> v1 <= v2
      BooleanType -> false
      NumberType -> (readFloat v1) <= (readFloat v2)

    isGT::String->String->AttributeType->Boolean
    isGT v1 v2 t = case t of
      TextType -> v1 > v2
      BooleanType -> false
      NumberType -> (readFloat v1) > (readFloat v2)

    isGTE::String->String->AttributeType->Boolean
    isGTE v1 v2 t = case t of
      TextType -> v1 >= v2
      BooleanType -> false
      NumberType -> (readFloat v1) >= (readFloat v2)

    isIN::String->String->String->AttributeType->Boolean
    isIN v1 v21 v22 t = case t of
      TextType -> v1 >= v21 && v1 <= v22
      BooleanType -> false
      NumberType -> (readFloat v1) > (readFloat v21) && (readFloat v1) < (readFloat v22)
