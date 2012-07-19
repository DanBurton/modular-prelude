{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude.LText
  ( LTextModule (..)
  , _Data_Text_Lazy_
  ) where


import ModularPrelude
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Filesystem.Path.CurrentOS as F


data LTextModule = TL
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


_Data_Text_Lazy_ :: LTextModule
_Data_Text_Lazy_ = TL
  { map       = TL.map
  , concatMap = TL.concatMap
  , filter    = TL.filter
  , length    = TL.length
  , singleton = TL.singleton
  , null      = TL.null
  , pack      = TL.pack
  , unpack    = TL.unpack
  , empty     = TL.empty
  , readFile  = TL.readFile . F.encodeString
  , writeFile = TL.writeFile . F.encodeString
  , break     = TL.break
  , span      = TL.span
  , dropWhile = TL.dropWhile
  , takeWhile = TL.takeWhile
  , any       = TL.any
  , all       = TL.all
  , splitAt   = TL.splitAt
  }


instance Default LTextModule where
  def = _Data_Text_Lazy_

