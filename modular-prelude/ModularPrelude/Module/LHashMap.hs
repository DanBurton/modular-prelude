{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.HashMap.Lazy" module.
-- 
-- Note that the data type for this module is still HashMap,
-- and not LHashMap.
module ModularPrelude.Module.LHashMap
  ( -- * Module interface
    LHashMapModule (..)
    -- * Module contents
  , LHashMapImplements (..)
  ) where


import ModularPrelude
import qualified Data.HashMap.Lazy as LHashMap


data LHashMapModule = LHashMap
  { map       :: forall k a b. (a -> b) -> HashMap k a -> HashMap k b
  , filter    :: forall k a. (k -> a -> Bool) -> HashMap k a -> HashMap k a
  , length    :: forall k a. HashMap k a -> Int
  , singleton :: forall k a. Hashable k => k -> a -> HashMap k a
  , null      :: forall k a. HashMap k a -> Bool
  , pack      :: forall k a. (Eq k, Hashable k) => [(k, a)] -> HashMap k a
  , unpack    :: forall k a. HashMap k a -> [(k, a)]
  , fromList  :: forall k a. (Eq k, Hashable k) => [(k, a)] -> HashMap k a
  , toList    :: forall k a. HashMap k a -> [(k, a)]
  , lookup    :: forall k a. (Eq k, Hashable k) => k -> HashMap k a -> Maybe a
  , empty     :: forall k a. HashMap k a
  , insert    :: forall k a. (Eq k, Hashable k) => k -> a -> HashMap k a -> HashMap k a
  , delete    :: forall k a. (Eq k, Hashable k) => k -> HashMap k a -> HashMap k a
  , member    :: forall k a. (Eq k, Hashable k) => k -> HashMap k a -> Bool
  }


class LHashMapImplements interface where
  _Data_HashMap_Lazy_ :: interface

instance LHashMapImplements LHashMapModule where
  _Data_HashMap_Lazy_ = LHashMap
    { map       = LHashMap.map
    , filter    = LHashMap.filterWithKey
    , length    = LHashMap.size
    , singleton = LHashMap.singleton
    , null      = LHashMap.null
    , pack      = LHashMap.fromList
    , unpack    = LHashMap.toList
    , fromList  = LHashMap.fromList
    , toList    = LHashMap.toList
    , lookup    = LHashMap.lookup
    , empty     = LHashMap.empty
    , insert    = LHashMap.insert
    , delete    = LHashMap.delete
    , member    = LHashMap.member
    }


instance Default LHashMapModule where
  def = _Data_HashMap_Lazy_

