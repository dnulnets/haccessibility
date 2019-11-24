{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Acessability.Model
-- Description : The database model
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the database model and the haskell representation of
-- the data    
--
module Accessability.Model (Item(..), migrateAll) where

--
-- Import standard libs
--
import Data.Text (Text)

--import Control.Monad.IO.Unlift (MonadUnliftIO)
--import Control.Monad.Logger (MonadLogger)

--import Data.Typeable (Typeable)
--import GHC.Generics (Generic)

--
-- Import for persistence
--
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
                              
--
-- The Model
--
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Item
    name String
    description String
    level Int
    UniqueItemName name
    deriving Show
|]
