{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Acessability.Data.User
-- Description : The user type that are used by the application
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the user information regardless of interface.
--
module Accessability.Data.User
    ( User(..)
    , Operation(..)
    , UserProperty(..)
    )
where

--
-- Import standard libs
--
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic(..) )

--
-- Import for persistence
--
import           Database.Persist.TH

--
-- JSON library
--
import           Data.Aeson
import           Data.Aeson.TH

--
-- Our own stuff
--
import           Accessability.Utils.JSON       ( firstLower )
import           Accessability.Data.Item        (AttributeType)
--
-- JSON Option
--
customOptions :: Options
customOptions = defaultOptions

--
-- User
--

-- | Definition of the user
data User = User {
    userId            :: Maybe Text -- ^ User key
    , userUsername        :: Text       -- ^ The username used when looging in
    , userPassword    :: Text       -- ^ The password, bcrypted
    , userEmail       :: Text       -- ^ The user email address
    } deriving (Generic)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 4 -- Get rid of the 'user' in the field names
  } ''User)

--
-- User Property
--

-- |The property operation todetermine if they are accessible
data Operation = GT -- ^ Greater than value
    | GTE           -- ^ Greater than or equal to value
    | LT            -- ^ Less then value
    | LTE           -- ^ Less than or equal to value
    | EQ            -- ^ Equal to value
    | IN            -- ^ Inside range [value, value1]
    deriving (Generic, Show, Read, Eq)
--
-- Persistence for Enumberation AttributeType
--
derivePersistField "Operation"

-- JSON for the Operation
--

instance ToJSON Operation where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Operation where
  parseJSON = genericParseJSON customOptions

  -- | Definition of the property
data UserProperty = UserProperty {
    propertyUserPropertyId :: Text           -- ^UserProperty key
    , propertyAttributeId   :: Text      -- ^Attribute key
    , propertyName        :: Text            -- ^The name of the attribute
    , propertyDisplayName :: Text            -- ^The name to display for this attribute
    , propertyGroup       :: Text            -- ^The group the attribute belongs to
    , propertyDescription :: Text            -- ^The description of the attribute 
    , propertyTypeof      :: AttributeType   -- ^The type of the attribute
    , propertyUnit        :: Text            -- ^The unit of the attribute
    , propertyValue       :: Text            -- ^The value of the attribute
    , propertyValue1      :: Maybe Text      -- ^The other value, if this is a range
    , propertyOperation   :: Operation       -- ^The operation
    , propertyNegate      :: Bool            -- ^Logical negate
    } deriving (Generic, Show)

-- |Automatically derive JSON but we do not want the first charatcer in the field to go out
$(deriveJSON defaultOptions {
    fieldLabelModifier = firstLower . drop 8 -- Get rid of the 'property' in the field names
  } ''UserProperty)
