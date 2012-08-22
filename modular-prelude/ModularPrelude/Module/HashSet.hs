{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.HashSet" module.
module ModularPrelude.Module.HashSet
  ( -- * Module interface
    HashSetModule (..)
    -- * Module contents
  , HashSetImplements (..)
  ) where


import ModularPrelude
import qualified Data.HashSet as HashSet


data HashSetModule = HashSet
  { -- Construction
    empty       :: forall a. HashSet a
  , singleton   :: forall a. Hashable a => a -> HashSet a
    -- Combine
  , union       :: forall a. (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
  , unions      :: forall a. (Eq a, Hashable a) => [HashSet a] -> HashSet a
    -- Basic interface
  , null        :: forall a. HashSet a -> Bool
  , size        :: forall a. HashSet a -> Int
  , member      :: forall a. (Eq a, Hashable a) => a -> HashSet a -> Bool
  , insert      :: forall a. (Eq a, Hashable a) => a -> HashSet a -> HashSet a
  , delete      :: forall a. (Eq a, Hashable a) => a -> HashSet a -> HashSet a
    -- Transformations
  , map         :: forall a b. (Hashable b, Eq b) => (a -> b) -> HashSet a -> HashSet b
    -- Difference and intersection
  , difference  :: forall a. (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
  , intersection :: forall a. (Eq a, Hashable a) => HashSet a -> HashSet a -> HashSet a
    -- Folds
  , foldl'      :: forall a b. (a -> b -> a) -> a -> HashSet b -> a
  , foldr       :: forall a b. (b -> a -> a) -> a -> HashSet b -> a
    -- Filter
  , filter      :: forall a. (a -> Bool) -> HashSet a -> HashSet a
    -- Lists
  , toList      :: forall a. HashSet a -> [a]
  , fromList    :: forall a. (Eq a, Hashable a) => [a] -> HashSet a
  }


class HashSetImplements interface where
  _Data_HashSet_ :: interface

instance HashSetImplements HashSetModule where
  _Data_HashSet_ = HashSet
    { -- Construction
      empty       = HashSet.empty
    , singleton   = HashSet.singleton
      -- Combine
    , union       = HashSet.union
    , unions      = HashSet.unions
      -- Basic interface
    , null        = HashSet.null
    , size        = HashSet.size
    , member      = HashSet.member
    , insert      = HashSet.insert
    , delete      = HashSet.delete
      -- Transformations
    , map         = HashSet.map
      -- Difference and intersection
    , difference  = HashSet.difference
    , intersection = HashSet.intersection
      -- Folds
    , foldl'      = HashSet.foldl'
    , foldr       = HashSet.foldr
      -- Filter
    , filter      = HashSet.filter
      -- Lists
    , toList      = HashSet.toList
    , fromList    = HashSet.fromList
    }


instance Default HashSetModule where
  def = _Data_HashSet_

