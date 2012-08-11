{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude.FilePath
  ( FilePathModule (..)
  , _Filesystem_Path_CurrentOS_
  ) where


import Prelude (String)
import ModularPrelude
import qualified Filesystem.Path.CurrentOS as F


data FilePathModule = FilePath
  { pack        :: String -> FilePath
  , unpack      :: FilePath -> String
  , stripPrefix :: FilePath -> FilePath -> Maybe FilePath
  }


_Filesystem_Path_CurrentOS_ :: FilePathModule
_Filesystem_Path_CurrentOS_ = FilePath
  { pack        = F.decodeString
  , unpack      = F.encodeString
  , stripPrefix = F.stripPrefix
  }


instance Default FilePathModule where
  def = _Filesystem_Path_CurrentOS_

