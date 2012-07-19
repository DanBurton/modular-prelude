{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

module ModularPrelude.LHashMap
  ( LHashMapModule (..)
  , _Data_HashMap_Lazy_
  ) where


import ModularPrelude
import qualified Data.HashMap.Lazy as LHashMap


data LHashMapModule = LHashMap
  { map       :: forall k a b. (a -> b) -> LHashMap k a -> LHashMap k b
  , filter    :: forall k a. (k -> a -> Bool) -> LHashMap k a -> LHashMap k a
  , length    :: forall k a. LHashMap k a -> Int
  , singleton :: forall k a. Hashable k => k -> a -> LHashMap k a
  , null      :: forall k a. LHashMap k a -> Bool
  , pack      :: forall k a. (Eq k, Hashable k) => [(k, a)] -> LHashMap k a
  , unpack    :: forall k a. LHashMap k a -> [(k, a)]
  , fromList  :: forall k a. (Eq k, Hashable k) => [(k, a)] -> LHashMap k a
  , toList    :: forall k a. LHashMap k a -> [(k, a)]
  , lookup    :: forall k a. (Eq k, Hashable k) => k -> LHashMap k a -> Maybe a
  , empty     :: forall k a. LHashMap k a
  , insert    :: forall k a. (Eq k, Hashable k) => k -> a -> LHashMap k a -> LHashMap k a
  , delete    :: forall k a. (Eq k, Hashable k) => k -> LHashMap k a -> LHashMap k a
  , member    :: forall k a. (Eq k, Hashable k) => k -> LHashMap k a -> Bool
  }


_Data_HashMap_Lazy_ :: LHashMapModule
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

