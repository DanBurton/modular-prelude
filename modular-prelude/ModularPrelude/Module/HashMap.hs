{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.HashMap.Strict" module.
module ModularPrelude.Module.HashMap
  ( -- * Module interface
    HashMapModule (..)
    -- * Module contents
  , HashMapImplements (..)
  ) where


import ModularPrelude
import qualified Data.HashMap.Strict as HashMap


data HashMapModule = HashMap
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


class HashMapImplements interface where
  _Data_HashMap_Strict_ :: interface

instance HashMapImplements HashMapModule where
  _Data_HashMap_Strict_ = HashMap
    { -- Construction
      empty     = HashMap.empty
    , singleton = HashMap.singleton
      -- Basic interface
    , null      = HashMap.null
    , size      = HashMap.size
    , member    = HashMap.member
    , lookup    = HashMap.lookup
    , lookupDefault = HashMap.lookupDefault
    , (!)       = (HashMap.!)
    , insert    = HashMap.insert
    , insertWith = HashMap.insertWith
    , delete    = HashMap.delete
    , adjust    = HashMap.adjust
      -- Combine
      -- . Union
    , union     = HashMap.union
    , unionWith = HashMap.unionWith
    , unions    = HashMap.unions
      -- Transformations
    , map       = HashMap.map
    , traverseWithKey = HashMap.traverseWithKey
      -- Difference and intersection
    , difference = HashMap.difference
    , intersection = HashMap.intersection
    , intersectionWith = myIntersectionWith -- different!
      -- Folds
    , foldl'    = HashMap.foldl'
    , foldlWithKey' = HashMap.foldlWithKey'
    , foldr     = HashMap.foldr
    , foldrWithKey  = HashMap.foldrWithKey
      -- Filter
    , filter    = HashMap.filter
    , filterWithKey = HashMap.filterWithKey
      -- Conversions
    , keys      = HashMap.keys
    , elems     = HashMap.elems
      -- Lists
    , toList    = HashMap.toList
    , fromList  = HashMap.fromList
    , fromListWith  = HashMap.fromListWith
    }

-- Copied from unordered-containers 0.2.2.0
-- todo: cpp, inlinable
myIntersectionWith :: (Eq k, Hashable k) => (v1 -> v2 -> v3) -> HashMap k v1
                 -> HashMap k v2 -> HashMap k v3
myIntersectionWith f a b = HashMap.foldlWithKey' go HashMap.empty a where
  go m k v = case HashMap.lookup k b of
    Just w -> HashMap.insert k (f v w) m
    _      -> m

instance Default HashMapModule where
  def = _Data_HashMap_Strict_

