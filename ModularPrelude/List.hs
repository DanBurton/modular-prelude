{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

module ModularPrelude.List
  ( ListModule (..)
  , _Data_List_
  ) where


import ModularPrelude
import qualified Data.List as List


data ListModule = List
  { map         :: forall a b. (a -> b) -> [a] -> [b]
  , concatMap   :: forall a b. (a -> [b]) -> [a] -> [b]
  , filter      :: forall a. (a -> Bool) -> [a] -> [a]
  , length      :: forall a. [a] -> Int
  , singleton   :: forall a. a -> [a]
  , null        :: forall a. [a] -> Bool
  , pack        :: forall a. [a] -> [a]
  , unpack      :: forall a. [a] -> [a]
  , fromList    :: forall a. [a] -> [a]
  , toList      :: forall a. [a] -> [a]
  , lookup      :: forall k a. Eq k => k -> [(k, a)] -> Maybe a
  , empty       :: forall a. [a]
  , insert      :: forall a. a -> [a] -> [a]
  , delete      :: forall a. Eq a => a -> [a] -> [a]
  , member      :: forall a. Eq a => a -> [a] -> Bool
  , stripPrefix :: forall a. Eq a => [a] -> [a] -> Maybe [a]
  , break       :: forall a. (a -> Bool) -> [a] -> ([a], [a])
  , span        :: forall a. (a -> Bool) -> [a] -> ([a], [a])
  , dropWhile   :: forall a. (a -> Bool) -> [a] -> [a]
  , takeWhile   :: forall a. (a -> Bool) -> [a] -> [a]
  , any         :: forall a. (a -> Bool) -> [a] -> Bool
  , all         :: forall a. (a -> Bool) -> [a] -> Bool
  , splitAt     :: forall a. Int -> [a] -> ([a], [a])
  , fold        :: forall a b. (a -> b -> a) -> a -> [b] -> a
  }


_Data_List_ :: ListModule
_Data_List_ = List
  { map         = List.map
  , concatMap   = List.concatMap
  , filter      = List.filter
  , length      = List.length
  , singleton   = return
  , null        = List.null
  , pack        = id
  , unpack      = id
  , fromList    = id
  , toList      = id
  , lookup      = List.lookup
  , empty       = []
  , insert      = (:)
  , delete      = List.delete
  , member      = List.elem
  , stripPrefix = List.stripPrefix
  , break       = List.break
  , span        = List.span
  , dropWhile   = List.dropWhile
  , takeWhile   = List.takeWhile
  , any         = List.any
  , all         = List.all
  , splitAt     = List.splitAt
  , fold        = List.foldl'
  }


instance Default ListModule where
  def = _Data_List_

