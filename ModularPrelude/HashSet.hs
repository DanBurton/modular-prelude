{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

module ModularPrelude.HashSet
  ( HashSetModule (..)
  , _Data_HashSet_
  ) where


import ModularPrelude
import qualified Data.HashSet as HashSet


data HashSetModule = HashSet
  { map         :: forall a b. (Hashable b, Eq b) => (a -> b) -> HashSet a -> HashSet b
  , filter      :: forall a. (a -> Bool) -> HashSet a -> HashSet a
  , length      :: forall a. HashSet a -> Int
  , singleton   :: forall a. Hashable a => a -> HashSet a
  , null        :: forall a. HashSet a -> Bool
  , pack        :: forall a. (Eq a, Hashable a) => [a] -> HashSet a
  , unpack      :: forall a. HashSet a -> [a]
  , fromList    :: forall a. (Eq a, Hashable a) => [a] -> HashSet a
  , toList      :: forall a. HashSet a -> [a]
  , empty       :: forall a. HashSet a
  , insert      :: forall a. (Eq a, Hashable a) => a -> HashSet a -> HashSet a
  , delete      :: forall a. (Eq a, Hashable a) => a -> HashSet a -> HashSet a
  , member      :: forall a. (Eq a, Hashable a) => a -> HashSet a -> Bool
  }


_Data_HashSet_ :: HashSetModule
_Data_HashSet_ = HashSet
  { map         = HashSet.map
  , filter      = HashSet.filter
  , length      = HashSet.size
  , singleton   = HashSet.singleton
  , null        = HashSet.null
  , pack        = HashSet.fromList
  , unpack      = HashSet.toList
  , fromList    = HashSet.fromList
  , toList      = HashSet.toList
  , empty       = HashSet.empty
  , insert      = HashSet.insert
  , delete      = HashSet.delete
  , member      = HashSet.member
  }


instance Default HashSetModule where
  def = _Data_HashSet_

