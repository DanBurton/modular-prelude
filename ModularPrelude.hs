{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude
    ( module X
    , LHashMap
    , UVector
    ) where

import BasicPrelude as X
import Data.Default as X

import Data.Vector.Unboxed as X (Unbox)

import qualified Data.HashMap.Lazy
import qualified Data.Vector.Unboxed

type LHashMap = Data.HashMap.Lazy.HashMap
type UVector = Data.Vector.Unboxed.Vector
