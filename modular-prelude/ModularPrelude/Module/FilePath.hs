{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a first-class version
-- of the "Filesystem.Path.CurrentOS" module.
module ModularPrelude.Module.FilePath
  ( -- Module interface
    FilePathModule (..)
    -- Module contents
  , FilePathImplements (..)
  ) where


import Prelude (String)
import ModularPrelude
import qualified Filesystem.Path.CurrentOS as FilePath


data FilePathModule = FilePath
  { pack        :: String -> FilePath
  , unpack      :: FilePath -> String
  , stripPrefix :: FilePath -> FilePath -> Maybe FilePath
  }


class FilePathImplements interface where
  _Filesystem_Path_CurrentOS_ :: interface

instance FilePathImplements FilePathModule where
  _Filesystem_Path_CurrentOS_ = FilePath
    { pack        = FilePath.decodeString
    , unpack      = FilePath.encodeString
    , stripPrefix = FilePath.stripPrefix
    }


instance Default FilePathModule where
  def = _Filesystem_Path_CurrentOS_

