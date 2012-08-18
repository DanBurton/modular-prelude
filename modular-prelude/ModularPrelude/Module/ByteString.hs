{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a first-class version
-- of the "Data.ByteString" module.
module ModularPrelude.Module.ByteString
  ( -- * Module interface
    ByteStringModule (..)
    -- * Module contents
  , ByteStringImplements (..)
  ) where


import ModularPrelude
import qualified Data.ByteString as ByteString
import qualified Filesystem.Path.CurrentOS as FilePath


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


class ByteStringImplements interface where
  _Data_ByteString_ :: interface

instance ByteStringImplements ByteStringModule where
  _Data_ByteString_ = ByteString
    { map       = ByteString.map
    , concatMap = ByteString.concatMap
    , filter    = ByteString.filter
    , length    = ByteString.length
    , singleton = ByteString.singleton
    , null      = ByteString.null
    , pack      = ByteString.pack
    , unpack    = ByteString.unpack
    , empty     = ByteString.empty
    , readFile  = ByteString.readFile . FilePath.encodeString
    , writeFile = ByteString.writeFile . FilePath.encodeString
    , break     = ByteString.break
    , span      = ByteString.span
    , dropWhile = ByteString.dropWhile
    , takeWhile = ByteString.takeWhile
    , any       = ByteString.any
    , all       = ByteString.all
    , splitAt   = ByteString.splitAt
    }


instance Default ByteStringModule where
  def = _Data_ByteString_

