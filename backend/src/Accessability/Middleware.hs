{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Heat.Middleware
-- Description : The applications middlewares
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the CORS Middleware.
module Accessability.Middleware
  ( corsified
  )
where

-- Wai imports
import           Network.Wai                    ( Middleware )
import           Network.Wai.Middleware.Cors    ( CorsResourcePolicy(..)
                                                , cors
                                                )

-- |The CORS middleware
corsified :: Middleware -- ^Returns with the middleware
corsified = cors (const $ Just corsPolicy)

-- | CORS resource policy to be used with 'corsified' middleware.
corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
