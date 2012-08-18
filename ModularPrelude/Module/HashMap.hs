{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.HashMap.Strict" module.
module ModularPrelude.Module.HashMap
  ( -- * Module interface
    HashMapModule (..)
    -- * Module contents
  , HashMapImplements (..)
  ) where


import ModularPrelude hiding (empty)
import qualified Data.HashMap.Strict as HashMap


data HashMapModule = HashMap
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


class HashMapImplements interface where
  _Data_HashMap_Strict_ :: interface

instance HashMapImplements HashMapModule where
  _Data_HashMap_Strict_ = HashMap
    { map       = HashMap.map
    , filter    = HashMap.filterWithKey
    , length    = HashMap.size
    , singleton = HashMap.singleton
    , null      = HashMap.null
    , pack      = HashMap.fromList
    , unpack    = HashMap.toList
    , fromList  = HashMap.fromList
    , toList    = HashMap.toList
    , lookup    = HashMap.lookup
    , empty     = HashMap.empty
    , insert    = HashMap.insert
    , delete    = HashMap.delete
    , member    = HashMap.member
    }


instance Default HashMapModule where
  def = _Data_HashMap_Strict_

