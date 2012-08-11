{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude
    ( module X
    , LHashMap
    ) where

import BasicPrelude as X
import Data.Default as X

import qualified Data.HashMap.Lazy
type LHashMap = Data.HashMap.Lazy.HashMap

