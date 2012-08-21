{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a first-class version
-- of the "Filesystem.Path.CurrentOS" module.
module ModularPrelude.Module.FilePath
  ( -- Module interface
    FilePathModule (..)
    -- Module contents
  , FilePathImplements (..)
  ) where


import ModularPrelude hiding (filename, basename, (</>), hasExtension, (<.>))
import Filesystem.Path.Rules (Rules)
import qualified Filesystem.Path.CurrentOS as FilePath


data FilePathModule = FilePath
  { -- Re-exported Filesystem.Path
    empty     :: FilePath
    -- . Basic properties
  , null      :: FilePath -> Bool
  , root      :: FilePath -> FilePath
  , directory :: FilePath -> FilePath
  , parent    :: FilePath -> FilePath
  , filename  :: FilePath -> FilePath
  , dirname   :: FilePath -> FilePath
  , basename  :: FilePath -> FilePath
  , absolute  :: FilePath -> Bool
  , relative  :: FilePath -> Bool
    -- . Basic operations
  , append    :: FilePath -> FilePath -> FilePath
  , (</>)     :: FilePath -> FilePath -> FilePath
  , concat    :: [FilePath] -> FilePath
  , commonPrefix :: [FilePath] -> FilePath
  , stripPrefix  :: FilePath -> FilePath -> Maybe FilePath
  , collapse  :: FilePath -> FilePath
    -- . Extensions
  , extension         :: FilePath -> Maybe Text
  , extensions        :: FilePath -> [Text]
  , hasExtension      :: FilePath -> Text -> Bool
  , addExtension      :: FilePath -> Text -> FilePath
  , (<.>)             :: FilePath -> Text -> FilePath
  , dropExtension     :: FilePath -> FilePath
  , replaceExtension  :: FilePath -> Text -> FilePath
  , addExtensions     :: FilePath -> [Text] -> FilePath
  , dropExtensions    :: FilePath -> FilePath
  , replaceExtensions :: FilePath -> [Text] -> FilePath
  , splitExtension    :: FilePath -> (FilePath, Maybe Text)
  , splitExtensions   :: FilePath -> (FilePath, [Text])
    -- Filesystem.Path.CurrentOS
  , currentOS         :: Rules ByteString
    -- Type conversions
  , toText            :: FilePath -> Either Text Text
  , fromText          :: Text -> FilePath
  , encode            :: FilePath -> ByteString
  , decode            :: ByteString -> FilePath
  , encodeString      :: FilePath -> String
  , decodeString      :: String -> FilePath
    -- Rule-specific path properties
  , valid             :: FilePath -> Bool
  , splitSearchPath   :: ByteString -> [FilePath]
  }


class FilePathImplements interface where
  _Filesystem_Path_CurrentOS_ :: interface

instance FilePathImplements FilePathModule where
  _Filesystem_Path_CurrentOS_ = FilePath
    { -- Re-exported Filesystem.Path
      empty     = FilePath.empty
      -- . Basic properties
    , null      = FilePath.null
    , root      = FilePath.root
    , directory = FilePath.directory
    , parent    = FilePath.parent
    , filename  = FilePath.filename
    , dirname   = FilePath.dirname
    , basename  = FilePath.basename
    , absolute  = FilePath.absolute
    , relative  = FilePath.relative
      -- . Basic operations
    , append    = FilePath.append
    , (</>)     = (FilePath.</>)
    , concat    = FilePath.concat
    , commonPrefix = FilePath.commonPrefix
    , stripPrefix  = FilePath.stripPrefix
    , collapse  = FilePath.collapse
      -- . Extensions
    , extension         = FilePath.extension
    , extensions        = FilePath.extensions
    , hasExtension      = FilePath.hasExtension
    , addExtension      = FilePath.addExtension
    , (<.>)             = (FilePath.<.>)
    , dropExtension     = FilePath.dropExtension
    , replaceExtension  = FilePath.replaceExtension
    , addExtensions     = FilePath.addExtensions
    , dropExtensions    = FilePath.dropExtensions
    , replaceExtensions = FilePath.replaceExtensions
    , splitExtension    = FilePath.splitExtension
    , splitExtensions   = FilePath.splitExtensions
      -- Filesystem.Path.CurrentOS
    , currentOS         = FilePath.currentOS
      -- Type conversions
    , toText            = FilePath.toText
    , fromText          = FilePath.fromText
    , encode            = FilePath.encode
    , decode            = FilePath.decode
    , encodeString      = FilePath.encodeString
    , decodeString      = FilePath.decodeString
      -- Rule-specific path properties
    , valid             = FilePath.valid
    , splitSearchPath   = FilePath.splitSearchPath
    }

instance Default FilePathModule where
  def = _Filesystem_Path_CurrentOS_

