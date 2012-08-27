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
  { -- Construction
    empty     :: forall k v. HashMap k v
  , singleton :: forall k v. Hashable k => k -> v -> HashMap k v
    -- Basic interface
  , null      :: forall k v. HashMap k v -> Bool
  , size      :: forall k v. HashMap k v -> Int
  , member    :: forall k v. (Eq k, Hashable k) => k -> HashMap k v -> Bool
  , lookup    :: forall k v. (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
  , lookupDefault :: forall k v. (Eq k, Hashable k) => v -> k -> HashMap k v -> v
  , (!)       :: forall k v. (Eq k, Hashable k) => HashMap k v -> k -> v
  , insert    :: forall k v. (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
  , insertWith :: forall k v. (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
  , delete    :: forall k v. (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
  , adjust    :: forall k v. (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
    -- Combine
    -- . Union
  , union     :: forall k v. (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
  , unionWith :: forall k v. (Eq k, Hashable k) => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
  , unions    :: forall k v. (Eq k, Hashable k) => [HashMap k v] -> HashMap k v
    -- Transformations
  , map       :: forall k v1 v2. (v1 -> v2) -> HashMap k v1 -> HashMap k v2
  , traverseWithKey :: forall k v1 v2 f. Applicative f => (k -> v1 -> f v2) -> HashMap k v1 -> f (HashMap k v2)
    -- Difference and intersection
  , difference :: forall k v. (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
  , intersection :: forall k v. (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
  , intersectionWith :: forall k v1 v2 v3. (Eq k, Hashable k) => (v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
    -- Folds
  , foldl'    :: forall k v a. (a -> v -> a) -> a -> HashMap k v -> a
  , foldlWithKey' :: forall k v a. (a -> k -> v -> a) -> a -> HashMap k v -> a
  , foldr     :: forall k v a. (v -> a -> a) -> a -> HashMap k v -> a
  , foldrWithKey :: forall k v a. (k -> v -> a -> a) -> a -> HashMap k v -> a
    -- Filter
  , filter    :: forall k v. (v -> Bool) -> HashMap k v -> HashMap k v
  , filterWithKey :: forall k v. (k -> v -> Bool) -> HashMap k v -> HashMap k v
    -- Conversions
  , keys      :: forall k v. HashMap k v -> [k]
  , elems     :: forall k v. HashMap k v -> [v]
    -- Lists
  , toList    :: forall k v. HashMap k v -> [(k, v)]
  , fromList  :: forall k v. (Eq k, Hashable k) => [(k, v)] -> HashMap k v
  , fromListWith :: forall k v. (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
  }



class LHashMapImplements interface where
  _Data_HashMap_Lazy_ :: interface

instance LHashMapImplements LHashMapModule where
  _Data_HashMap_Lazy_ = LHashMap
    { -- Construction
      empty     = LHashMap.empty
    , singleton = LHashMap.singleton
      -- Basic interface
    , null      = LHashMap.null
    , size      = LHashMap.size
    , member    = LHashMap.member
    , lookup    = LHashMap.lookup
    , lookupDefault = LHashMap.lookupDefault
    , (!)       = (LHashMap.!)
    , insert    = LHashMap.insert
    , insertWith = LHashMap.insertWith
    , delete    = LHashMap.delete
    , adjust    = LHashMap.adjust
      -- Combine
      -- . Union
    , union     = LHashMap.union
    , unionWith = LHashMap.unionWith
    , unions    = LHashMap.unions
      -- Transformations
    , map       = LHashMap.map
    , traverseWithKey = LHashMap.traverseWithKey
      -- Difference and intersection
    , difference = LHashMap.difference
    , intersection = LHashMap.intersection
    , intersectionWith = myIntersectionWith -- different!
      -- Folds
    , foldl'    = LHashMap.foldl'
    , foldlWithKey' = LHashMap.foldlWithKey'
    , foldr     = LHashMap.foldr
    , foldrWithKey  = LHashMap.foldrWithKey
      -- Filter
    , filter    = LHashMap.filter
    , filterWithKey = LHashMap.filterWithKey
      -- Conversions
    , keys      = LHashMap.keys
    , elems     = LHashMap.elems
      -- Lists
    , toList    = LHashMap.toList
    , fromList  = LHashMap.fromList
    , fromListWith  = LHashMap.fromListWith
    }


-- Copied from unordered-containers 0.2.2.0
-- todo: cpp, inlinable
myIntersectionWith :: (Eq k, Hashable k) => (v1 -> v2 -> v3) -> HashMap k v1
                 -> HashMap k v2 -> HashMap k v3
myIntersectionWith f a b = LHashMap.foldlWithKey' go LHashMap.empty a where
  go m k v = case LHashMap.lookup k b of
    Just w -> LHashMap.insert k (f v w) m
    _      -> m

instance Default LHashMapModule where
  def = _Data_HashMap_Lazy_

