{-# LANGUAGE NoImplicitPrelude, PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.List" module.
module ModularPrelude.Module.List
  ( -- * Module interface
    ListModule (..)
    -- * Module contents
  , ListImplements (..)
  ) where


import ModularPrelude
import qualified Data.List as List


data ListModule = List
  { -- Basic Functions
    (++)         :: forall a. [a] -> [a] -> [a]
  , head         :: forall a. [a] -> a
  , last         :: forall a. [a] -> a
  , tail         :: forall a. [a] -> [a]
  , init         :: forall a. [a] -> [a]
  , null         :: forall a. [a] -> Bool
  , length       :: forall a. [a] -> Int
    -- List transformations
  , map          :: forall a b. (a -> b) -> [a] -> [b]
  , reverse      :: forall a. [a] -> [a]
  , intersperse  :: forall a. a -> [a] -> [a]
  , intercalate  :: forall a. [a] -> [[a]] -> [a]
  , transpose    :: forall a. [[a]] -> [[a]]
  , subsequences :: forall a. [a] -> [[a]]
  , permutations :: forall a. [a] -> [[a]]
    -- Reducing lists (folds)
  , foldl        :: forall a b. (a -> b -> a) -> a -> [b] -> a
  , foldl'       :: forall a b. (a -> b -> a) -> a -> [b] -> a
  , foldl1       :: forall a. (a -> a -> a) -> [a] -> a
  , foldl1'      :: forall a. (a -> a -> a) -> [a] -> a
  , foldr        :: forall a b. (a -> b -> b) -> b -> [a] -> b
  , foldr1       :: forall a. (a -> a -> a) -> [a] -> a
    -- . Special folds
  , concat       :: forall a. [[a]] -> [a]
  , concatMap   :: forall a b. (a -> [b]) -> [a] -> [b]
  , and         :: [Bool] -> Bool
  , or          :: [Bool] -> Bool
  , any         :: forall a. (a -> Bool) -> [a] -> Bool
  , all         :: forall a. (a -> Bool) -> [a] -> Bool
  , sum         :: Num a => [a] -> a
  , product     :: Num a => [a] -> a
  , maximum     :: Ord a => [a] -> a
  , minimum     :: Ord a => [a] -> a
    -- Building lists
    -- . Scans
  , scanl       :: forall a b. (a -> b -> a) -> a -> [b] -> [a]
  , scanl1      :: forall a. (a -> a -> a) -> [a] -> [a]
  , scanr       :: forall a b. (a -> b -> b) -> b -> [a] -> [b]
  , scanr1      :: forall a. (a -> a -> a) -> [a] -> [a]
    -- . Accumulating maps
  , mapAccumL   :: forall acc x y. (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
  , mapAccumR   :: forall acc x y. (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    -- . Infinite lists
  , iterate     :: forall a. (a -> a) -> a -> [a]
  , repeat      :: forall a. a -> [a]
  , replicate   :: forall a. Int -> a -> [a]
  , cycle       :: forall a. [a] -> [a]
    -- . Unfolding
  , unfoldr     :: forall a b. (b -> Maybe (a, b)) -> b -> [a]
    -- Sublists
    -- . Extracting sublists
  , take        :: forall a. Int -> [a] -> [a]
  , drop        :: forall a. Int -> [a] -> [a]
  , splitAt     :: forall a. Int -> [a] -> ([a], [a])
  , takeWhile   :: forall a. (a -> Bool) -> [a] -> [a]
  , dropWhile   :: forall a. (a -> Bool) -> [a] -> [a]
  , dropWhileEnd :: forall a. (a -> Bool) -> [a] -> [a]
  , span        :: forall a. (a -> Bool) -> [a] -> ([a], [a])
  , break       :: forall a. (a -> Bool) -> [a] -> ([a], [a])
  , stripPrefix :: forall a. Eq a => [a] -> [a] -> Maybe [a]
  , group       :: forall a. Eq a => [a] -> [[a]]
  , inits       :: forall a. [a] -> [[a]]
  , tails       :: forall a. [a] -> [[a]]
    -- . Predicates
  , isPrefixOf  :: forall a. Eq a => [a] -> [a] -> Bool
  , isSuffixOf  :: forall a. Eq a => [a] -> [a] -> Bool
  , isInfixOf   :: forall a. Eq a => [a] -> [a] -> Bool
    -- Searching lists
    -- . Searching by equality
  , elem        :: forall a. Eq a => a -> [a] -> Bool
  , notElem     :: forall a. Eq a => a -> [a] -> Bool
  , lookup      :: forall a b. Eq a => a -> [(a, b)] -> Maybe b
    -- . Searching with a predicate
  , find        :: forall a. (a -> Bool) -> [a] -> Maybe a
  , filter      :: forall a. (a -> Bool) -> [a] -> [a]
  , partition   :: forall a. (a -> Bool) -> [a] -> ([a], [a])
    -- Indexing lists
  , (!!)        :: forall a. [a] -> Int -> a
  , elemIndex   :: forall a. Eq a => a -> [a] -> Maybe Int
  , elemIndices :: forall a. Eq a => a -> [a] -> [Int]
  , findIndex   :: forall a. (a -> Bool) -> [a] -> Maybe Int
  , findIndices :: forall a. (a -> Bool) -> [a] -> [Int]
    -- Zipping and unzipping lists
  , zip         :: forall a b. [a] -> [b] -> [(a, b)]
  , zip3        :: forall a b c. [a] -> [b] -> [c] -> [(a, b, c)]
  , zip4        :: forall a b c d. [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
  , zip5        :: forall a b c d e. [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
  , zip6        :: forall a b c d e f. [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
  , zip7        :: forall a b c d e f g. [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
  , zipWith     :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
  , zipWith3    :: forall a b c d. (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
  , zipWith4    :: forall a b c d e. (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
  , zipWith5    :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
  , zipWith6    :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
  , zipWith7    :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
  , unzip       :: forall a b. [(a, b)] -> ([a], [b])
  , unzip3      :: forall a b c. [(a, b, c)] -> ([a], [b], [c])
  , unzip4      :: forall a b c d. [(a, b, c, d)] -> ([a], [b], [c], [d])
  , unzip5      :: forall a b c d e. [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
  , unzip6      :: forall a b c d e f. [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
  , unzip7      :: forall a b c d e f g. [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
    -- Special lists
    -- . Functions on strings
  , lines       :: String -> [String]
  , words       :: String -> [String]
  , unlines     :: [String] -> String
  , unwords     :: [String] -> String
    -- . "Set" operations
  , nub         :: forall a. Eq a => [a] -> [a]
  , delete      :: forall a. Eq a => a -> [a] -> [a]
  , (\\)        :: forall a. Eq a => [a] -> [a] -> [a]
  , union       :: forall a. Eq a => [a] -> [a] -> [a]
  , intersect   :: forall a. Eq a => [a] -> [a] -> [a]
    -- . Ordered lists
  , sort        :: forall a. Ord a => [a] -> [a]
  , insert      :: forall a. Ord a => a -> [a] -> [a]
    -- Generalized functions
    -- . The "By" operations
    -- . . User-supplied equality (replacing an Eq context)
  , nubBy       :: forall a. (a -> a -> Bool) -> [a] -> [a]
  , deleteBy    :: forall a. (a -> a -> Bool) -> a -> [a] -> [a]
  , deleteFirstsBy :: forall a. (a -> a -> Bool) -> [a] -> [a] -> [a]
  , unionBy     :: forall a. (a -> a -> Bool) -> [a] -> [a] -> [a]
  , intersectBy :: forall a. (a -> a -> Bool) -> [a] -> [a] -> [a]
  , groupBy     :: forall a. (a -> a -> Bool) -> [a] -> [[a]]
    -- . . User-supplied comparison (replacing an Ord context)
  , sortBy      :: forall a. (a -> a -> Ordering) -> [a] -> [a]
  , insertBy    :: forall a. (a -> a -> Ordering) -> a -> [a] -> [a]
  , maximumBy   :: forall a. (a -> a -> Ordering) -> [a] -> a
  , minimumBy   :: forall a. (a -> a -> Ordering) -> [a] -> a
    -- . The "generic" operations
  , genericLength    :: forall i b. Num i => [b] -> i
  , genericTake      :: forall i a. Integral i => i -> [a] -> [a]
  , genericDrop      :: forall i a. Integral i => i -> [a] -> [a]
  , genericSplitAt   :: forall i b. Integral i => i -> [b] -> ([b], [b])
  , genericIndex     :: forall a b. Integral a => [b] -> a -> b
  , genericReplicate :: forall i a. Integral i => i -> a -> [a]
  }


class ListImplements interface where
  _Data_List_ :: interface

instance ListImplements ListModule where
  _Data_List_ = List
    { -- Basic functions
      (++) = (List.++)
    , head = List.head
    , last = List.last
    , tail = List.tail
    , init = List.init
    , null = List.null
    , length = List.length
      -- List transformations
    , map          = List.map
    , reverse      = List.reverse
    , intersperse  = List.intersperse
    , intercalate  = List.intercalate
    , transpose    = List.transpose
    , subsequences = List.subsequences
    , permutations = List.permutations
      -- Reducing lists (folds)
    , foldl   = List.foldl
    , foldl'  = List.foldl'
    , foldl1  = List.foldl1
    , foldl1' = List.foldl1'
    , foldr   = List.foldr
    , foldr1  = List.foldr1
      -- . Special folds
    , concat    = List.concat
    , concatMap = List.concatMap
    , and       = List.and
    , or        = List.or
    , any       = List.any
    , all       = List.all
    , sum       = List.sum
    , product   = List.product
    , maximum   = List.maximum
    , minimum   = List.minimum
      -- Building lists
      -- . Scans
    , scanl  = List.scanl
    , scanl1 = List.scanl1
    , scanr  = List.scanr
    , scanr1 = List.scanr1
      -- . Accumulating maps
    , mapAccumL = List.mapAccumL
    , mapAccumR = List.mapAccumR
      -- . Infinite lists
    , iterate   = List.iterate
    , repeat    = List.repeat
    , replicate = List.replicate
    , cycle     = List.cycle
      -- . Unfolding
    , unfoldr = List.unfoldr
      -- Sublists
      -- . Extracting sublists
    , take         = List.take
    , drop         = List.drop
    , splitAt      = List.splitAt
    , takeWhile    = List.takeWhile
    , dropWhile    = List.dropWhile
    , dropWhileEnd = myDropWhileEnd -- different!
    , span         = List.span
    , break        = List.break
    , stripPrefix  = List.stripPrefix
    , group        = List.group
    , inits        = List.inits
    , tails        = List.tails
      -- . Predicates
    , isPrefixOf = List.isPrefixOf
    , isSuffixOf = List.isSuffixOf
    , isInfixOf  = List.isInfixOf
      -- Searching lists
      -- . Searching by equality
    , elem    = List.elem
    , notElem = List.notElem
    , lookup  = List.lookup
      -- . Searching with a predicate
    , find      = List.find
    , filter    = List.filter
    , partition = List.partition
      -- Indexing lists
    , (!!) = (List.!!)
    , elemIndex   = List.elemIndex
    , elemIndices = List.elemIndices
    , findIndex   = List.findIndex
    , findIndices = List.findIndices
      -- Zipping and unzipping lists
    , zip  = List.zip
    , zip3 = List.zip3
    , zip4 = List.zip4
    , zip5 = List.zip5
    , zip6 = List.zip6
    , zip7 = List.zip7
    , zipWith  = List.zipWith
    , zipWith3 = List.zipWith3
    , zipWith4 = List.zipWith4
    , zipWith5 = List.zipWith5
    , zipWith6 = List.zipWith6
    , zipWith7 = List.zipWith7
    , unzip  = List.unzip
    , unzip3 = List.unzip3
    , unzip4 = List.unzip4
    , unzip5 = List.unzip5
    , unzip6 = List.unzip6
    , unzip7 = List.unzip7
      -- Special lists
      -- . Functions on strings
    , lines   = List.lines
    , words   = List.words
    , unlines = List.unlines
    , unwords = List.unwords
      -- . "Set" operations
    , nub       = List.nub
    , delete    = List.delete
    , (\\)      = (List.\\)
    , union     = List.union
    , intersect = List.intersect
      -- . Ordered lists
    , sort   = List.sort
    , insert = List.insert
      -- Generalized functions
      -- . The "By" operations
      -- . . User-supplied equality (replacing an Eq context)
    , nubBy          = List.nubBy
    , deleteBy       = List.deleteBy
    , deleteFirstsBy = List.deleteFirstsBy
    , unionBy        = List.unionBy
    , intersectBy    = List.intersectBy
    , groupBy        = List.groupBy
      -- . . User-supplied comparison (replacing an Ord context)
    , sortBy    = List.sortBy
    , insertBy  = List.insertBy
    , maximumBy = List.maximumBy
    , minimumBy = List.minimumBy
      -- . The "generic" operations
    , genericLength    = List.genericLength
    , genericTake      = List.genericTake
    , genericDrop      = List.genericDrop
    , genericSplitAt   = List.genericSplitAt
    , genericIndex     = List.genericIndex
    , genericReplicate = List.genericReplicate
    }

-- copied from base 4.5
-- TODO: CPP the List definition in if base >= 4.5 ?
myDropWhileEnd :: (a -> Bool) -> [a] -> [a]
myDropWhileEnd p = List.foldr step [] where
  step x xs = if p x && List.null xs then [] else x : xs

instance Default ListModule where
  def = _Data_List_

