{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ModularPrelude.Classy
  ( ClassyModule (..)
  , _ClassyPrelude_Classes_
  ) where


import ModularPrelude
import Prelude (Read) -- This should be in CorePrelude
import qualified ClassyPrelude as Classy
import qualified ClassyPrelude.Classes as Classy
import ClassyPrelude () -- Make sure we get all instances
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
  , CanWords
  , CanSplit
  , CanStripSuffix
  , CanIsInfixOf
  , CanReverse
  , CanReplicate
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
  , mapM        :: CanMapM    f m i o => (i -> m o) -> f
  , mapM_       :: CanMapM_   f m i   => (i -> m o) -> f
  , lookup      :: CanLookup    c k v => k -> c -> Maybe v
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
  , take        :: CanSplitAt   c i   => i -> c -> c
  , drop        :: CanSplitAt   c i   => i -> c -> c
  , fold        :: CanFold  f a accum => (accum -> a -> accum) -> accum -> f
  , words       :: CanWords       t   => t -> [t]
  , unwords     :: CanWords       t   => [t] -> t
  , lines       :: CanWords       t   => t -> [t]
  , unlines     :: CanWords       t   => [t] -> t
  , split       :: CanSplit     c i   => (i -> Bool) -> c -> [c]
  , stripSuffix :: CanStripSuffix a   => a -> a -> Maybe a
  , isSuffixOf  :: CanStripSuffix a   => a -> a -> Bool
  , isInfixOf   :: CanIsInfixOf   a   => a -> a -> Bool
  , reverse     :: CanReverse     a   => a -> a
  , replicate   :: CanReplicate a i l => l -> i -> a
  , fromList    :: CanPack      c i   => [i] -> c
  , toList      :: CanPack      c i   => c -> [i]
  , show        :: (Show a, CanPack c Char) => a -> c  
  , readMay     :: (Read b, CanPack a Char) => a -> Maybe b
  , repack      :: (CanPack a i, CanPack b i) => a -> b
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
  , mapM        = Classy.mapM
  , mapM_       = Classy.mapM_
  , lookup      = Classy.lookup
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
  , take        = Classy.take
  , drop        = Classy.drop
  , fold        = Classy.fold
  , words       = Classy.words
  , unwords     = Classy.unwords
  , lines       = Classy.lines
  , unlines     = Classy.unlines
  , split       = Classy.split
  , stripSuffix = Classy.stripSuffix
  , isSuffixOf  = Classy.isSuffixOf
  , isInfixOf   = Classy.isInfixOf
  , reverse     = Classy.reverse
  , replicate   = Classy.replicate
  , fromList    = Classy.fromList
  , toList      = Classy.toList
  , show        = Classy.show
  , readMay     = Classy.readMay
  , repack      = Classy.repack
  }


instance Default ClassyModule where
  def = _ClassyPrelude_Classes_

