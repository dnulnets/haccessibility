{-# LANGUAGE ExplicitForAll #-}

-- |
-- Module      : Acessability.Data.Functor
-- Description : Some generic functor functions to handle nested functors
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains some functions that makes it easier to handle nested functors
--
module Accessability.Data.Functor (
    ffmap,
    fffmap) where

-- Here comes an ugly section of nested fmaps, need to rethink this if there is a better way

-- | We are drilling three layers down into a functor, should perhaps be written
-- a bit differently to avoid the need for this, but here we are
fffmap::(Functor e) => (Functor f) => (Functor g) => (a->b) -> e (f (g a)) -> e (f (g b))
fffmap = fmap . fmap . fmap

-- | We are drilling two layers down into a functor, should perhaps be written
-- a bit differently to avoid the need for this, but here we are
ffmap::(Functor f) => (Functor g) => (a->b) -> f (g a) -> f (g b)
ffmap = fmap . fmap
