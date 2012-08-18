{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a first-class version
-- of the "Data.Text" module.
module ModularPrelude.Module.Text
  ( -- * Module interface
    TextModule (..)
    -- * Module contents
  , TextImplements (..)
  ) where


import ModularPrelude
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Filesystem.Path.CurrentOS as FilePath


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


class TextImplements interface where
  _Data_Text_ :: interface

instance TextImplements TextModule where
  _Data_Text_ = Text
    { map       = Text.map
    , concatMap = Text.concatMap
    , filter    = Text.filter
    , length    = Text.length
    , singleton = Text.singleton
    , null      = Text.null
    , pack      = Text.pack
    , unpack    = Text.unpack
    , empty     = Text.empty
    , readFile  = Text.readFile . FilePath.encodeString
    , writeFile = Text.writeFile . FilePath.encodeString
    , break     = Text.break
    , span      = Text.span
    , dropWhile = Text.dropWhile
    , takeWhile = Text.takeWhile
    , any       = Text.any
    , all       = Text.all
    , splitAt   = Text.splitAt
    }


instance Default TextModule where
  def = _Data_Text_

