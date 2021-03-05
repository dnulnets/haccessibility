

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings   #-}

-- |
-- Module      : Acessability.Data.Analysis
-- Description : Functions for analysing POI value with respect to user properties
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the common item type regardless of interface or database
-- that is associated with geographical items.
--
module Accessability.Data.Analysis (evaluatePOI) where

--
-- Import standard libs
--
import           Prelude

import qualified Data.Map                as Map
import           Data.Maybe              (Maybe (..), fromMaybe)
import           Data.Text               (Text, unpack)

import Control.Monad (join)

import           Accessability.Data.Item (Attribute (..), AttributeType (..),
                                          ItemValue (..))
import qualified Accessability.Data.User as U

-- |Determines the value of a POI based on the users properties
evaluatePOI::[U.UserProperty] -> [Attribute] -> ItemValue
evaluatePOI aup aav = mconcat (evaluateUserProperty (toAttributeValueMap aav) <$> aup )

  where

    toAttributeValueMap::[Attribute] -> Map.Map Text Attribute
    toAttributeValueMap a = Map.fromList $ (\av->(fromMaybe "" (attributeAttributeId av), av)) <$> a

    evaluateUserProperty::Map.Map Text Attribute -> U.UserProperty -> ItemValue
    evaluateUserProperty msa up = case join $ Map.lookup <$> Just (U.propertyAttributeId up) <*> Just msa of
                      Nothing -> ItemValue {positive = 0, negative = 0, unknown = 1, positiveAttributes = [], negativeAttributes = [], unknownAttributes = [U.propertyDisplayName up]}
                      Just a -> if evaluate up a
                                  then ItemValue {positive = 1, negative = 0, unknown = 0, positiveAttributes = [attributeDisplayName a], negativeAttributes = [], unknownAttributes = []}
                                  else ItemValue {positive = 0, negative = 1, unknown = 0, positiveAttributes = [], negativeAttributes = [attributeDisplayName a], unknownAttributes = []}

    evaluate::U.UserProperty->Attribute->Bool
    evaluate up av = Just True == (notit <$> Just (U.propertyNegate up)
                                            <*> (operate  <$> Just (U.propertyOperation up)
                                                          <*> attributeValue av
                                                          <*> Just (U.propertyValue up)
                                                          <*> Just (U.propertyValue1 up)
                                                          <*> Just (attributeTypeof av)))

    -- Logical xor
    notit::Bool->Bool->Bool
    notit True v  = not v
    notit False v = v

    operate::U.Operation->Text->Text->Maybe Text->AttributeType->Bool
    operate U.EQ v1 v2 _ TextType = v1 == v2
    operate U.EQ v1 v2 _ BooleanType = v1 == v2
    operate U.EQ v1 v2 _ NumberType = ((read $ unpack v1)::Float) == read (unpack v2)
    operate U.LT v1 v2 _ TextType = v1 < v2
    operate U.LT _ _ _ BooleanType = False
    operate U.LT v1 v2 _ NumberType = ((read $ unpack v1)::Float) < read (unpack v2)
    operate U.LTE v1 v2 _ TextType = v1 <= v2
    operate U.LTE _ _ _ BooleanType = False
    operate U.LTE v1 v2 _ NumberType = ((read $ unpack v1)::Float) <= read (unpack v2)
    operate U.GT v1 v2 _ TextType = v1 > v2
    operate U.GT _ _ _ BooleanType = False
    operate U.GT v1 v2 _ NumberType = ((read $ unpack v1)::Float) > read (unpack v2)
    operate U.GTE v1 v2 _ TextType = v1 >= v2
    operate U.GTE _ _ _ BooleanType = False
    operate U.GTE v1 v2 _ NumberType = ((read $ unpack v1)::Float) >= read (unpack v2)
    operate U.IN v1 v21 (Just v22) TextType = v1 >= v21 && v1 <= v22
    operate U.IN _ _ (Just _) BooleanType = False
    operate U.IN v1 v21 (Just v22) NumberType = ((read $ unpack v1)::Float) > read (unpack v21) && ((read $ unpack v1)::Float) < read (unpack v22)
    operate U.IN _ _ Nothing _ = False
