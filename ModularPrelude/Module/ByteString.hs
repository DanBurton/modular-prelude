{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude.Module.ByteString
  ( ByteStringModule (..)
  , _Data_ByteString_
  ) where


import ModularPrelude hiding (empty)
import qualified Data.ByteString as S
import qualified Filesystem.Path.CurrentOS as F


data ByteStringModule = ByteString
  { map       :: (Word8 -> Word8) -> ByteString -> ByteString
  , concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
  , filter    :: (Word8 -> Bool) -> ByteString -> ByteString
  , length    :: ByteString -> Int
  , singleton :: Word8 -> ByteString
  , null      :: ByteString -> Bool
  , pack      :: [Word8] -> ByteString
  , unpack    :: ByteString -> [Word8]
  , empty     :: ByteString
  , readFile  :: FilePath -> IO ByteString
  , writeFile :: FilePath -> ByteString -> IO ()
  , break     :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
  , span      :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
  , dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
  , takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
  , any       :: (Word8 -> Bool) -> ByteString -> Bool
  , all       :: (Word8 -> Bool) -> ByteString -> Bool
  , splitAt   :: Int -> ByteString -> (ByteString, ByteString)
  }


_Data_ByteString_ :: ByteStringModule
_Data_ByteString_ = ByteString
  { map       = S.map
  , concatMap = S.concatMap
  , filter    = S.filter
  , length    = S.length
  , singleton = S.singleton
  , null      = S.null
  , pack      = S.pack
  , unpack    = S.unpack
  , empty     = S.empty
  , readFile  = S.readFile . F.encodeString
  , writeFile = S.writeFile . F.encodeString
  , break     = S.break
  , span      = S.span
  , dropWhile = S.dropWhile
  , takeWhile = S.takeWhile
  , any       = S.any
  , all       = S.all
  , splitAt   = S.splitAt
  }


instance Default ByteStringModule where
  def = _Data_ByteString_

