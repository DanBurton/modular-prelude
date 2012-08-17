{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

module ModularPrelude.Vector
  ( VectorModule (..)
  , _Data_Vector_
  ) where


import ModularPrelude hiding (empty)
import qualified Data.Vector as V


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
  , dropWhile :: forall a.(a -> Bool) -> Vector a -> Vector a
  , takeWhile :: forall a.(a -> Bool) -> Vector a -> Vector a
  , any       :: forall a.(a -> Bool) -> Vector a -> Bool
  , all       :: forall a.(a -> Bool) -> Vector a -> Bool
  , splitAt   :: forall a.Int -> Vector a -> (Vector a, Vector a)
  , fold      :: forall a b. (a -> b -> a) -> a -> Vector b -> a
  }


_Data_Vector_ :: VectorModule
_Data_Vector_ = Vector
  { map       = V.map
  , concatMap = V.concatMap
  , filter    = V.filter
  , length    = V.length
  , singleton = V.singleton
  , null      = V.null
  , pack      = V.fromList
  , unpack    = V.toList
  , fromList  = V.fromList
  , toList    = V.toList
  , mapM      = V.mapM
  , mapM_     = V.mapM_
  , empty     = V.empty
  , member    = V.any . (==)
  , break     = V.break
  , span      = V.span
  , dropWhile = V.dropWhile
  , takeWhile = V.takeWhile
  , any       = V.any
  , all       = V.all
  , splitAt   = V.splitAt
  , fold      = V.foldl'
  }


instance Default VectorModule where
  def = _Data_Vector_

