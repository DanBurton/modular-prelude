{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ModularPrelude.Classy
  ( ClassyModule (..)
  , _ClassyPrelude_Classes_
  ) where


import ModularPrelude
import qualified ClassyPrelude.Classes as Classy
import ClassyPrelude ()
import ClassyPrelude.Classes
  ( CanMap
  , CanConcatMap
  , CanFilter
  , CanLength
  , CanSingleton
  , CanNull
  , CanPack
  , CanMapM
  , CanMapM_
  , CanLookup
  , CanEmpty
  , CanInsert
  , CanDelete
  , CanMember
  , CanReadFile
  , CanWriteFile
  , CanStripPrefix
  , CanBreak
  , CanAny
  , CanSplitAt
  , CanFold
  )


data ClassyModule = Classy
  { map         :: CanMap       f i o => (i -> o) -> f
  , concatMap   :: CanConcatMap f i o => (i -> o) -> f
  , filter      :: CanFilter    f a   => (a -> Bool) -> f
  , length      :: CanLength    c i   => c -> i
  , singleton   :: CanSingleton c i   => i -> c
  , null        :: CanNull      c     => c -> Bool
  , pack        :: CanPack      c i   => [i] -> c
  , unpack      :: CanPack      c i   => c -> [i]
  , fromList    :: CanPack      c i   => [i] -> c
  , toList      :: CanPack      c i   => c -> [i]
  , mapM        :: CanMapM    f i o m => (i -> m o) -> f
  , mapM_       :: CanMapM_   f i o m => (i -> m o) -> f
  , lookup      :: CanLookup    c k v => k -> c -> Maybe v
  , empty       :: CanEmpty     c     => c
  , insert      :: CanInsert    f     => f
  , delete      :: CanDelete    c k   => k -> c -> c
  , member      :: CanMember    c k   => k -> c -> Bool
  , readFile    :: CanReadFile    a   => FilePath -> a
  , writeFile   :: CanWriteFile   a   => FilePath -> a
  , stripPrefix :: CanStripPrefix a   => a -> a -> Maybe a
  , break       :: CanBreak     c i   => (i -> Bool) -> c -> (c, c)
  , span        :: CanBreak     c i   => (i -> Bool) -> c -> (c, c)
  , dropWhile   :: CanBreak     c i   => (i -> Bool) -> c -> c
  , takeWhile   :: CanBreak     c i   => (i -> Bool) -> c -> c
  , any         :: CanAny       c i   => (i -> Bool) -> c -> Bool
  , all         :: CanAny       c i   => (i -> Bool) -> c -> Bool
  , splitAt     :: CanSplitAt   c i   => i -> c -> (c, c)
  , fold        :: CanFold  accum a f => (accum -> a -> accum) -> accum -> f
  }


_ClassyPrelude_Classes_ :: ClassyModule
_ClassyPrelude_Classes_ = Classy
  { map         = Classy.map
  , concatMap   = Classy.concatMap
  , filter      = Classy.filter
  , length      = Classy.length
  , singleton   = Classy.singleton
  , null        = Classy.null
  , pack        = Classy.pack
  , unpack      = Classy.unpack
  , fromList    = Classy.pack
  , toList      = Classy.unpack
  , mapM        = Classy.mapM
  , mapM_       = Classy.mapM_
  , lookup      = Classy.lookup
  , empty       = Classy.empty
  , insert      = Classy.insert
  , delete      = Classy.delete
  , member      = Classy.member
  , readFile    = Classy.readFile
  , writeFile   = Classy.writeFile
  , stripPrefix = Classy.stripPrefix
  , break       = Classy.break
  , span        = Classy.span
  , dropWhile   = Classy.dropWhile
  , takeWhile   = Classy.takeWhile
  , any         = Classy.any
  , all         = Classy.all
  , splitAt     = Classy.splitAt
  , fold        = Classy.fold
  }


instance Default ClassyModule where
  def = _ClassyPrelude_Classes_

