{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude.Module.Text
  ( TextModule (..)
  , _Data_Text_
  ) where


import ModularPrelude hiding (empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Filesystem.Path.CurrentOS as F


data TextModule = Text
  { map       :: (Char -> Char) -> Text -> Text
  , concatMap :: (Char -> Text) -> Text -> Text
  , filter    :: (Char -> Bool) -> Text -> Text
  , length    :: Text -> Int
  , singleton :: Char -> Text
  , null      :: Text -> Bool
  , pack      :: [Char] -> Text
  , unpack    :: Text -> [Char]
  , empty     :: Text
  , readFile  :: FilePath -> IO Text
  , writeFile :: FilePath -> Text -> IO ()
  , break     :: (Char -> Bool) -> Text -> (Text, Text)
  , span      :: (Char -> Bool) -> Text -> (Text, Text)
  , dropWhile :: (Char -> Bool) -> Text -> Text
  , takeWhile :: (Char -> Bool) -> Text -> Text
  , any       :: (Char -> Bool) -> Text -> Bool
  , all       :: (Char -> Bool) -> Text -> Bool
  , splitAt   :: Int -> Text -> (Text, Text)
  }


_Data_Text_ :: TextModule
_Data_Text_ = Text
  { map       = T.map
  , concatMap = T.concatMap
  , filter    = T.filter
  , length    = T.length
  , singleton = T.singleton
  , null      = T.null
  , pack      = T.pack
  , unpack    = T.unpack
  , empty     = T.empty
  , readFile  = T.readFile . F.encodeString
  , writeFile = T.writeFile . F.encodeString
  , break     = T.break
  , span      = T.span
  , dropWhile = T.dropWhile
  , takeWhile = T.takeWhile
  , any       = T.any
  , all       = T.all
  , splitAt   = T.splitAt
  }


instance Default TextModule where
  def = _Data_Text_

