{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.Map" module.
module ModularPrelude.Module.Map
  ( -- * Module interface
    MapModule (..)
    -- * Module contents
  , MapImplements (..)
  ) where


import ModularPrelude hiding (empty)
import qualified Data.Map as Map


data MapModule = Map
  { map       :: forall k a b. (a -> b) -> Map k a -> Map k b
  , filter    :: forall k a. Ord k => (k -> a -> Bool) -> Map k a -> Map k a
  , length    :: forall k a. Map k a -> Int
  , singleton :: forall k a. k -> a -> Map k a
  , null      :: forall k a. Map k a -> Bool
  , pack      :: forall k a. Ord k => [(k, a)] -> Map k a
  , unpack    :: forall k a. Map k a -> [(k, a)]
  , fromList  :: forall k a. Ord k => [(k, a)] -> Map k a
  , toList    :: forall k a. Map k a -> [(k, a)]
  , lookup    :: forall k a. Ord k => k -> Map k a -> Maybe a
  , empty     :: forall k a. Map k a
  , insert    :: forall k a. Ord k => k -> a -> Map k a -> Map k a
  , delete    :: forall k a. Ord k => k -> Map k a -> Map k a
  , member    :: forall k a. Ord k => k -> Map k a -> Bool
  }


class MapImplements interface where
  _Data_Map_ :: interface

instance MapImplements MapModule where
  _Data_Map_ = Map
    { map       = Map.map
    , filter    = Map.filterWithKey
    , length    = Map.size
    , singleton = Map.singleton
    , null      = Map.null
    , pack      = Map.fromList
    , unpack    = Map.toList
    , fromList  = Map.fromList
    , toList    = Map.toList
    , lookup    = Map.lookup
    , empty     = Map.empty
    , insert    = Map.insert
    , delete    = Map.delete
    , member    = Map.member
    }


instance Default MapModule where
  def = _Data_Map_

