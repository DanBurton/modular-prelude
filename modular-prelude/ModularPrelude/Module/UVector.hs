{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.Vector.Unboxed" module.
module ModularPrelude.Module.UVector
  ( -- * Module interface
    UVectorModule (..)
    -- * Module contents
  , UVectorImplements (..)
  ) where


import ModularPrelude
import qualified Data.Vector.Unboxed as UVector


data UVectorModule = UVector
  { map       :: forall a b. (Unbox a, Unbox b)
                                   => (a -> b) -> UVector a -> UVector b
  , concatMap :: forall a b. (Unbox a, Unbox b)
                                   => (a -> UVector b) -> UVector a -> UVector b
  , filter    :: forall a. Unbox a => (a -> Bool) -> UVector a -> UVector a
  , length    :: forall a. Unbox a => UVector a -> Int
  , singleton :: forall a. Unbox a => a -> UVector a
  , null      :: forall a. Unbox a => UVector a -> Bool
  , pack      :: forall a. Unbox a => [a] -> UVector a
  , unpack    :: forall a. Unbox a => UVector a -> [a]
  , fromList  :: forall a. Unbox a => [a] -> UVector a
  , toList    :: forall a. Unbox a => UVector a -> [a]
  , mapM      :: forall a b m. (Unbox a, Unbox b, Monad m)
                                   => (a -> m b) -> UVector a -> m (UVector b)
  , mapM_     :: forall a b m. (Unbox a, Unbox b, Monad m)
                                   => (a -> m b) -> UVector a -> m ()
  , empty     :: forall a. Unbox a => UVector a
  , member    :: forall a. Unbox a => Eq a => a -> UVector a -> Bool
  , break     :: forall a. Unbox a => (a -> Bool) -> UVector a -> (UVector a, UVector a)
  , span      :: forall a. Unbox a => (a -> Bool) -> UVector a -> (UVector a, UVector a)
  , dropWhile :: forall a. Unbox a => (a -> Bool) -> UVector a -> UVector a
  , takeWhile :: forall a. Unbox a => (a -> Bool) -> UVector a -> UVector a
  , any       :: forall a. Unbox a => (a -> Bool) -> UVector a -> Bool
  , all       :: forall a. Unbox a => (a -> Bool) -> UVector a -> Bool
  , splitAt   :: forall a. Unbox a => Int -> UVector a -> (UVector a, UVector a)
  , fold      :: forall a b. (Unbox a, Unbox b)
                                   => (a -> b -> a) -> a -> UVector b -> a
  }


class UVectorImplements interface where
  _Data_Vector_Unboxed_ :: interface

instance UVectorImplements UVectorModule where
  _Data_Vector_Unboxed_ = UVector
    { map       = UVector.map
    , concatMap = UVector.concatMap
    , filter    = UVector.filter
    , length    = UVector.length
    , singleton = UVector.singleton
    , null      = UVector.null
    , pack      = UVector.fromList
    , unpack    = UVector.toList
    , fromList  = UVector.fromList
    , toList    = UVector.toList
    , mapM      = UVector.mapM
    , mapM_     = UVector.mapM_
    , empty     = UVector.empty
    , member    = UVector.any . (==)
    , break     = UVector.break
    , span      = UVector.span
    , dropWhile = UVector.dropWhile
    , takeWhile = UVector.takeWhile
    , any       = UVector.any
    , all       = UVector.all
    , splitAt   = UVector.splitAt
    , fold      = UVector.foldl'
    }


instance Default UVectorModule where
  def = _Data_Vector_Unboxed_

