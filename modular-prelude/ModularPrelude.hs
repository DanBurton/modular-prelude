{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides the core functions
-- and type declarations necessary to conveniently interact
-- with the standard modules provided with modular-prelude.
-- 
-- It is recommended that you always import this module
-- when using other modules from modular-prelude.
module ModularPrelude
    ( module CorePrelude
    , module Data.Default
    , Prelude.String
    ) where

import CorePrelude
import Data.Default

import qualified Prelude

