{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

module ModularPrelude.Set
  ( SetModule (..)
  , _Data_Set_
  ) where


import ModularPrelude
import qualified Data.Set as Set


data SetModule = Set
  { map         :: forall a b. (Ord a, Ord b) => (a -> b) -> Set a -> Set b
  , filter      :: forall a. (a -> Bool) -> Set a -> Set a
  , length      :: forall a. Set a -> Int
  , singleton   :: forall a. a -> Set a
  , null        :: forall a. Set a -> Bool
  , pack        :: forall a. Ord a => [a] -> Set a
  , unpack      :: forall a. Set a -> [a]
  , fromList    :: forall a. Ord a => [a] -> Set a
  , toList      :: forall a. Set a -> [a]
  , empty       :: forall a. Set a
  , insert      :: forall a. Ord a => a -> Set a -> Set a
  , delete      :: forall a. Ord a => a -> Set a -> Set a
  , member      :: forall a. Ord a => a -> Set a -> Bool
  }


_Data_Set_ :: SetModule
_Data_Set_ = Set
  { map         = Set.map
  , filter      = Set.filter
  , length      = Set.size
  , singleton   = Set.singleton
  , null        = Set.null
  , pack        = Set.fromList
  , unpack      = Set.toList
  , fromList    = Set.fromList
  , toList      = Set.toList
  , empty       = Set.empty
  , insert      = Set.insert
  , delete      = Set.delete
  , member      = Set.member
  }


instance Default SetModule where
  def = _Data_Set_

