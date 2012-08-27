{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolymorphicComponents #-}

-- | This module provides a first-class version
-- of the "Data.ByteString" module.
module ModularPrelude.Module.ByteString
  ( -- * Module interface
    ByteStringModule (..)
    -- * Module contents
  , ByteStringImplements (..)
  ) where


import ModularPrelude hiding (putStrLn)
import qualified Data.ByteString as ByteString
import qualified Filesystem.Path.CurrentOS as FilePath
import System.IO (Handle)
import Foreign.C.String (CString, CStringLen)

data ByteStringModule = ByteString
  { -- Introducing and eliminating ByteStrings
    empty :: ByteString
  , singleton :: Word8 -> ByteString
  , pack :: [Word8] -> ByteString
  , unpack :: ByteString -> [Word8]
    -- Basic interface
  , cons :: Word8 -> ByteString -> ByteString
  , snoc :: ByteString -> Word8 -> ByteString
  , append :: ByteString -> ByteString -> ByteString
  , head :: ByteString -> Word8
  , uncons :: ByteString -> Maybe (Word8, ByteString)
  , last :: ByteString -> Word8
  , tail :: ByteString -> ByteString
  , init :: ByteString -> ByteString
  , null :: ByteString -> Bool
  , length :: ByteString -> Int
    -- Transforming ByteStrings
  , map :: (Word8 -> Word8) -> ByteString -> ByteString
  , reverse :: ByteString -> ByteString
  , intersperse :: Word8 -> ByteString -> ByteString
  , intercalate :: ByteString -> [ByteString] -> ByteString
  , transpose :: [ByteString] -> [ByteString]
    -- Reducing ByteStrings (folds)
  , foldl :: forall a. (a -> Word8 -> a) -> a -> ByteString -> a
  , foldl' :: forall a. (a -> Word8 -> a) -> a -> ByteString -> a
  , foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
  , foldl1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
  , foldr :: forall a. (Word8 -> a -> a) -> a -> ByteString -> a
  , foldr' :: forall a. (Word8 -> a -> a) -> a -> ByteString -> a
  , foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
  , foldr1' :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
    -- Special folds
  , concat :: [ByteString] -> ByteString
  , concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
  , any :: (Word8 -> Bool) -> ByteString -> Bool
  , all :: (Word8 -> Bool) -> ByteString -> Bool
  , maximum :: ByteString -> Word8
  , minimum :: ByteString -> Word8
    -- Building ByteStrings
    -- . Scans
  , scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
  , scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
  , scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
  , scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
    -- . Accumulating maps
  , mapAccumL :: forall acc. (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
  , mapAccumR :: forall acc. (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
    -- . Generating and unfolding ByteStrings
  , replicate :: Int -> Word8 -> ByteString
  , unfoldr :: forall a. (a -> Maybe (Word8, a)) -> a -> ByteString
  , unfoldrN :: forall a. Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
    -- Substrings
    -- . Breaking strings
  , take :: Int -> ByteString -> ByteString
  , drop :: Int -> ByteString -> ByteString
  , splitAt :: Int -> ByteString -> (ByteString, ByteString)
  , takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
  , dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
  , span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
  , spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
  , break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
  , breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
  , group :: ByteString -> [ByteString]
  , groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
  , inits :: ByteString -> [ByteString]
  , tails :: ByteString -> [ByteString]
    -- . Breaking into many substrings
  , split :: Word8 -> ByteString -> [ByteString]
  , splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
    -- Predicates
  , isPrefixOf :: ByteString -> ByteString -> Bool
  , isSuffixOf :: ByteString -> ByteString -> Bool
  , isInfixOf :: ByteString -> ByteString -> Bool
    -- . Search for arbitrary substrings
  , breakSubstring :: ByteString -> ByteString -> (ByteString, ByteString)
  , findSubstring :: ByteString -> ByteString -> Maybe Int
  , findSubstrings :: ByteString -> ByteString -> [Int]
    -- Searching ByteStrings
    -- . Searching by equality
  , elem :: Word8 -> ByteString -> Bool
  , notElem :: Word8 -> ByteString -> Bool
    -- Searching with a predicate
  , find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
  , filter :: (Word8 -> Bool) -> ByteString -> ByteString
  , partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
    -- Indexing ByteStrings
  , index :: ByteString -> Int -> Word8
  , elemIndex :: Word8 -> ByteString -> Maybe Int
  , elemIndices :: Word8 -> ByteString -> [Int]
  , elemIndexEnd :: Word8 -> ByteString -> Maybe Int
  , findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
  , findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
  , count :: Word8 -> ByteString -> Int
    -- Zipping and unzipping ByteStrings
  , zip :: ByteString -> ByteString -> [(Word8, Word8)]
  , zipWith :: forall a. (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
  , unzip :: [(Word8, Word8)] -> (ByteString, ByteString)
    -- Ordered ByteStrings
  , sort :: ByteString -> ByteString
    -- Low level conversions
    -- . Copying ByteStrings
  , copy :: ByteString -> ByteString
    -- . Packing CStrings and pointers
  , packCString :: CString -> IO ByteString
  , packCStringLen :: CStringLen -> IO ByteString
    -- . Using ByteStrings as CStrings
  , useAsCString :: forall a. ByteString -> (CString -> IO a) -> IO a
  , useAsCStringLen :: forall a. ByteString -> (CStringLen -> IO a) -> IO a
    -- I/O with ByteStrings
    -- . Standard input and output
  , getLine :: IO ByteString
  , getContents :: IO ByteString
  , putStr :: ByteString -> IO ()
  , putStrLn :: ByteString -> IO ()
  , interact :: (ByteString -> ByteString) -> IO ()
    -- . Files
  , readFile :: String -> IO ByteString
  , writeFile :: String -> ByteString -> IO ()
  , appendFile :: String -> ByteString -> IO ()
    -- . I/O with Handles
  , hGetLine :: Handle -> IO ByteString
  , hGetContents :: Handle -> IO ByteString
  , hGet :: Handle -> Int -> IO ByteString
  , hGetSome :: Handle -> Int -> IO ByteString
  , hGetNonBlocking :: Handle -> Int -> IO ByteString
  , hPut :: Handle -> ByteString -> IO ()
  , hPutNonBlocking :: Handle -> ByteString -> IO ByteString
  , hPutStr :: Handle -> ByteString -> IO ()
  , hPutStrLn :: Handle -> ByteString -> IO ()
  , breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
  }


class ByteStringImplements interface where
  _Data_ByteString_ :: interface

instance ByteStringImplements ByteStringModule where
  _Data_ByteString_ = ByteString
    { -- Introducing and eliminating ByteStrings
      empty = ByteString.empty
    , singleton = ByteString.singleton
    , pack = ByteString.pack
    , unpack = ByteString.unpack
      -- Basic interface
    , cons = ByteString.cons
    , snoc = ByteString.snoc
    , append = ByteString.append
    , head = ByteString.head
    , uncons = ByteString.uncons
    , last = ByteString.last
    , tail = ByteString.tail
    , init = ByteString.init
    , null = ByteString.null
    , length = ByteString.length
      -- Transforming ByteStrings
    , map = ByteString.map
    , reverse = ByteString.reverse
    , intersperse = ByteString.intersperse
    , intercalate = ByteString.intercalate
    , transpose = ByteString.transpose
      -- Reducing ByteStrings (folds)
    , foldl = ByteString.foldl
    , foldl' = ByteString.foldl'
    , foldl1 = ByteString.foldl1
    , foldl1' = ByteString.foldl1'
    , foldr = ByteString.foldr
    , foldr' = ByteString.foldr'
    , foldr1 = ByteString.foldr1
    , foldr1' = ByteString.foldr1'
      -- Special folds
    , concat = ByteString.concat
    , concatMap = ByteString.concatMap
    , any = ByteString.any
    , all = ByteString.all
    , maximum = ByteString.maximum
    , minimum = ByteString.minimum
      -- Building ByteStrings
      -- . Scans
    , scanl = ByteString.scanl
    , scanl1 = ByteString.scanl1
    , scanr = ByteString.scanr
    , scanr1 = ByteString.scanr1
      -- . Accumulating maps
    , mapAccumL = ByteString.mapAccumL
    , mapAccumR = ByteString.mapAccumR
      -- . Generating and unfolding ByteStrings
    , replicate = ByteString.replicate
    , unfoldr = ByteString.unfoldr
    , unfoldrN = ByteString.unfoldrN
      -- Substrings
      -- . Breaking strings
    , take = ByteString.take
    , drop = ByteString.drop
    , splitAt = ByteString.splitAt
    , takeWhile = ByteString.takeWhile
    , dropWhile = ByteString.dropWhile
    , span = ByteString.span
    , spanEnd = ByteString.spanEnd
    , break = ByteString.break
    , breakEnd = ByteString.breakEnd
    , group = ByteString.group
    , groupBy = ByteString.groupBy
    , inits = ByteString.inits
    , tails = ByteString.tails
      -- . Breaking into many substrings
    , split = ByteString.split
    , splitWith = ByteString.splitWith
      -- Predicates
    , isPrefixOf = ByteString.isPrefixOf
    , isSuffixOf = ByteString.isSuffixOf
    , isInfixOf = ByteString.isInfixOf
      -- . Search for arbitrary substrings
    , breakSubstring = ByteString.breakSubstring
    , findSubstring = ByteString.findSubstring
    , findSubstrings = ByteString.findSubstrings
      -- Searching ByteStrings
      -- . Searching by equality
    , elem = ByteString.elem
    , notElem = ByteString.notElem
      -- Searching with a predicate
    , find = ByteString.find
    , filter = ByteString.filter
    , partition = ByteString.partition
      -- Indexing ByteStrings
    , index = ByteString.index
    , elemIndex = ByteString.elemIndex
    , elemIndices = ByteString.elemIndices
    , elemIndexEnd = ByteString.elemIndexEnd
    , findIndex = ByteString.findIndex
    , findIndices = ByteString.findIndices
    , count = ByteString.count
      -- Zipping and unzipping ByteStrings
    , zip = ByteString.zip
    , zipWith = ByteString.zipWith
    , unzip = ByteString.unzip
      -- Ordered ByteStrings
    , sort = ByteString.sort
      -- Low level conversions
      -- . Copying ByteStrings
    , copy = ByteString.copy
      -- . Packing CStrings and pointers
    , packCString = ByteString.packCString
    , packCStringLen = ByteString.packCStringLen
      -- . Using ByteStrings as CStrings
    , useAsCString = ByteString.useAsCString
    , useAsCStringLen = ByteString.useAsCStringLen
      -- I/O with ByteStrings
      -- . Standard input and output
    , getLine = ByteString.getLine
    , getContents = ByteString.getContents
    , putStr = ByteString.putStr
    , putStrLn = ByteString.putStrLn
    , interact = ByteString.interact
      -- Files
    , readFile = ByteString.readFile
    , writeFile = ByteString.writeFile
    , appendFile = ByteString.appendFile
      -- I/O with Handles
    , hGetLine = ByteString.hGetLine
    , hGetContents = ByteString.hGetContents
    , hGet = ByteString.hGet
    , hGetSome = ByteString.hGetSome
    , hGetNonBlocking = ByteString.hGetNonBlocking
    , hPut = ByteString.hPut
    , hPutNonBlocking = myHPutNonBlocking -- different!
    , hPutStr = ByteString.hPutStr
    , hPutStrLn = ByteString.hPutStrLn
    , breakByte = ByteString.breakByte
    }

-- This is cheating. TODO: cpp use the real version if available.
myHPutNonBlocking :: Handle -> ByteString -> IO ByteString
myHPutNonBlocking h bs = ByteString.hPut h bs >> return ByteString.empty

instance Default ByteStringModule where
  def = _Data_ByteString_

