{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.Vector" module.
module ModularPrelude.Module.Vector
  ( -- * Module interface
    VectorModule (..)
    -- * Module contents
  , VectorImplements (..)
  ) where


import ModularPrelude
import qualified Data.Vector as Vector


data VectorModule = Vector
  { map       :: forall a b. (a -> b) -> Vector a -> Vector b
  , concatMap :: forall a b. (a -> Vector b) -> Vector a -> Vector b
  , filter    :: forall a. (a -> Bool) -> Vector a -> Vector a
  , length    :: forall a. Vector a -> Int
  , singleton :: forall a. a -> Vector a
  , null      :: forall a. Vector a -> Bool
  , pack      :: forall a. [a] -> Vector a
  , unpack    :: forall a. Vector a -> [a]
  , fromList  :: forall a. [a] -> Vector a
  , toList    :: forall a. Vector a -> [a]
  , mapM      :: forall a b m. Monad m => (a -> m b) -> Vector a -> m (Vector b)
  , mapM_     :: forall a b m. Monad m => (a -> m b) -> Vector a -> m ()
  , empty     :: forall a. Vector a
  , member    :: forall a. Eq a => a -> Vector a -> Bool
  , break     :: forall a. (a -> Bool) -> Vector a -> (Vector a, Vector a)
  , span      :: forall a. (a -> Bool) -> Vector a -> (Vector a, Vector a)
  , dropWhile :: forall a. (a -> Bool) -> Vector a -> Vector a
  , takeWhile :: forall a. (a -> Bool) -> Vector a -> Vector a
  , any       :: forall a. (a -> Bool) -> Vector a -> Bool
  , all       :: forall a. (a -> Bool) -> Vector a -> Bool
  , splitAt   :: forall a. Int -> Vector a -> (Vector a, Vector a)
  , fold      :: forall a b. (a -> b -> a) -> a -> Vector b -> a
  }


class VectorImplements interface where
  _Data_Vector_ :: interface

instance VectorImplements VectorModule where
  _Data_Vector_ = Vector
    { map       = Vector.map
    , concatMap = Vector.concatMap
    , filter    = Vector.filter
    , length    = Vector.length
    , singleton = Vector.singleton
    , null      = Vector.null
    , pack      = Vector.fromList
    , unpack    = Vector.toList
    , fromList  = Vector.fromList
    , toList    = Vector.toList
    , mapM      = Vector.mapM
    , mapM_     = Vector.mapM_
    , empty     = Vector.empty
    , member    = Vector.any . (==)
    , break     = Vector.break
    , span      = Vector.span
    , dropWhile = Vector.dropWhile
    , takeWhile = Vector.takeWhile
    , any       = Vector.any
    , all       = Vector.all
    , splitAt   = Vector.splitAt
    , fold      = Vector.foldl'
    }


instance Default VectorModule where
  def = _Data_Vector_

