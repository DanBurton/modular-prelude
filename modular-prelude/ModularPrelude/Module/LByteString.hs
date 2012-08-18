{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a first-class version
-- of the "Data.ByteString.Lazy" module.
module ModularPrelude.Module.LByteString
  ( -- * Module interface
    LByteStringModule (..)
    -- * Module contents
  , LByteStringImplements (..)
  ) where


import ModularPrelude hiding (empty)
import qualified Data.ByteString.Lazy as LByteString
import qualified Filesystem.Path.CurrentOS as FilePath


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


class LByteStringImplements interface where
  _Data_ByteString_Lazy_ :: interface

instance LByteStringImplements LByteStringModule where
  _Data_ByteString_Lazy_ = LByteString
    { map       = LByteString.map
    , concatMap = LByteString.concatMap
    , filter    = LByteString.filter
    , length    = LByteString.length
    , singleton = LByteString.singleton
    , null      = LByteString.null
    , pack      = LByteString.pack
    , unpack    = LByteString.unpack
    , empty     = LByteString.empty
    , readFile  = LByteString.readFile . FilePath.encodeString
    , writeFile = LByteString.writeFile . FilePath.encodeString
    , break     = LByteString.break
    , span      = LByteString.span
    , dropWhile = LByteString.dropWhile
    , takeWhile = LByteString.takeWhile
    , any       = LByteString.any
    , all       = LByteString.all
    , splitAt   = LByteString.splitAt
    }


instance Default LByteStringModule where
  def = _Data_ByteString_Lazy_

