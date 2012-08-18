{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides the core type classes
-- from "ClassyPrelude" necessary to conveniently interact
-- with the standard modules provided with modular-prelude-classy.
-- 
-- It is recommended that you always import this module
-- -- *as well as "ModularPrelude"* --
-- when using other modules from modular-prelude-classy.
module ModularPrelude.Classy
  ( C.CanMap
  , C.CanConcatMap
  , C.CanFilter
  , C.CanLength
  , C.CanSingleton
  , C.CanNull
  , C.CanPack
  , C.CanMapM
  , C.CanMapM_
  , C.CanLookup
  , C.CanInsert
  , C.CanDelete
  , C.CanMember
  , C.CanReadFile
  , C.CanWriteFile
  , C.CanStripPrefix
  , C.CanBreak
  , C.CanAny
  , C.CanSplitAt
  , C.CanFold
  , C.CanWords
  , C.CanSplit
  , C.CanStripSuffix
  , C.CanIsInfixOf
  , C.CanReverse
  , C.CanReplicate
  ) where


import qualified ClassyPrelude.Classes as C

