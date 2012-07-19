{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

module ModularPrelude.UVector
  ( UVectorModule (..)
  , _Data_Vector_Unboxed_
  , UVector
  ) where


import ModularPrelude
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as UV

-- TODO: move this to BasicPrelude
type UVector = UV.Vector


data UVectorModule = UV
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


_Data_Vector_Unboxed_ :: UVectorModule
_Data_Vector_Unboxed_ = UV
  { map       = UV.map
  , concatMap = UV.concatMap
  , filter    = UV.filter
  , length    = UV.length
  , singleton = UV.singleton
  , null      = UV.null
  , pack      = UV.fromList
  , unpack    = UV.toList
  , fromList  = UV.fromList
  , toList    = UV.toList
  , mapM      = UV.mapM
  , mapM_     = UV.mapM_
  , empty     = UV.empty
  , member    = UV.any . (==)
  , break     = UV.break
  , span      = UV.span
  , dropWhile = UV.dropWhile
  , takeWhile = UV.takeWhile
  , any       = UV.any
  , all       = UV.all
  , splitAt   = UV.splitAt
  , fold      = UV.foldl'
  }


instance Default UVectorModule where
  def = _Data_Vector_Unboxed_

