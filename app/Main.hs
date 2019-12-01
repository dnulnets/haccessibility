-- |
-- Module      : Main
-- Description : The main entry point for the accessability grapql server
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@permobil.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the initialization and setup of the graphql API for the
-- accessability database.
--

module Main where

    import Boot
    
    main :: IO ()
    main = serverMain
