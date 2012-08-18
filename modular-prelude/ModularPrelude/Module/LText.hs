{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a first-class version
-- of the "Data.Text.Lazy" module.
module ModularPrelude.Module.LText
  ( -- * Module interface
    LTextModule (..)
    -- * Module contents
  , LTextImplements (..)
  ) where


import ModularPrelude hiding (empty)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Filesystem.Path.CurrentOS as FilePath


data LTextModule = LText
  { map       :: (Char -> Char) -> LText -> LText
  , concatMap :: (Char -> LText) -> LText -> LText
  , filter    :: (Char -> Bool) -> LText -> LText
  , length    :: LText -> Int64
  , singleton :: Char -> LText
  , null      :: LText -> Bool
  , pack      :: [Char] -> LText
  , unpack    :: LText -> [Char]
  , empty     :: LText
  , readFile  :: FilePath -> IO LText
  , writeFile :: FilePath -> LText -> IO ()
  , break     :: (Char -> Bool) -> LText -> (LText, LText)
  , span      :: (Char -> Bool) -> LText -> (LText, LText)
  , dropWhile :: (Char -> Bool) -> LText -> LText
  , takeWhile :: (Char -> Bool) -> LText -> LText
  , any       :: (Char -> Bool) -> LText -> Bool
  , all       :: (Char -> Bool) -> LText -> Bool
  , splitAt   :: Int64 -> LText -> (LText, LText)
  }


class LTextImplements interface where
  _Data_Text_Lazy_ :: interface

instance LTextImplements LTextModule where
  _Data_Text_Lazy_ = LText
    { map       = LText.map
    , concatMap = LText.concatMap
    , filter    = LText.filter
    , length    = LText.length
    , singleton = LText.singleton
    , null      = LText.null
    , pack      = LText.pack
    , unpack    = LText.unpack
    , empty     = LText.empty
    , readFile  = LText.readFile . FilePath.encodeString
    , writeFile = LText.writeFile . FilePath.encodeString
    , break     = LText.break
    , span      = LText.span
    , dropWhile = LText.dropWhile
    , takeWhile = LText.takeWhile
    , any       = LText.any
    , all       = LText.all
    , splitAt   = LText.splitAt
    }


instance Default LTextModule where
  def = _Data_Text_Lazy_

