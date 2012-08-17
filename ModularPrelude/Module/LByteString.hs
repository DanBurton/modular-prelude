{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude.Module.LByteString
  ( LByteStringModule (..)
  , _Data_ByteString_Lazy_
  ) where


import ModularPrelude hiding (empty)
import qualified Data.ByteString.Lazy as L
import qualified Filesystem.Path.CurrentOS as F


data LByteStringModule = LByteString
  { map       :: (Word8 -> Word8) -> LByteString -> LByteString
  , concatMap :: (Word8 -> LByteString) -> LByteString -> LByteString
  , filter    :: (Word8 -> Bool) -> LByteString -> LByteString
  , length    :: LByteString -> Int64
  , singleton :: Word8 -> LByteString
  , null      :: LByteString -> Bool
  , pack      :: [Word8] -> LByteString
  , unpack    :: LByteString -> [Word8]
  , empty     :: LByteString
  , readFile  :: FilePath -> IO LByteString
  , writeFile :: FilePath -> LByteString -> IO ()
  , break     :: (Word8 -> Bool) -> LByteString -> (LByteString, LByteString)
  , span      :: (Word8 -> Bool) -> LByteString -> (LByteString, LByteString)
  , dropWhile :: (Word8 -> Bool) -> LByteString -> LByteString
  , takeWhile :: (Word8 -> Bool) -> LByteString -> LByteString
  , any       :: (Word8 -> Bool) -> LByteString -> Bool
  , all       :: (Word8 -> Bool) -> LByteString -> Bool
  , splitAt   :: Int64 -> LByteString -> (LByteString, LByteString)
  }


_Data_ByteString_Lazy_ :: LByteStringModule
_Data_ByteString_Lazy_ = LByteString
  { map       = L.map
  , concatMap = L.concatMap
  , filter    = L.filter
  , length    = L.length
  , singleton = L.singleton
  , null      = L.null
  , pack      = L.pack
  , unpack    = L.unpack
  , empty     = L.empty
  , readFile  = L.readFile . F.encodeString
  , writeFile = L.writeFile . F.encodeString
  , break     = L.break
  , span      = L.span
  , dropWhile = L.dropWhile
  , takeWhile = L.takeWhile
  , any       = L.any
  , all       = L.all
  , splitAt   = L.splitAt
  }


instance Default LByteStringModule where
  def = _Data_ByteString_Lazy_

