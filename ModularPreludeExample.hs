{-# LANGUAGE NoImplicitPrelude #-}

import           ModularPrelude
import qualified ModularPrelude.From               as From


import qualified ModularPrelude.Module.ByteString  as Module
import qualified ModularPrelude.Module.LByteString as Module
import qualified ModularPrelude.Module.Text        as Module
import qualified ModularPrelude.Module.LText       as Module
import qualified ModularPrelude.Module.Vector      as Module
import qualified ModularPrelude.Module.UVector     as Module
import qualified ModularPrelude.Module.Map         as Module
import qualified ModularPrelude.Module.HashMap     as Module
import qualified ModularPrelude.Module.LHashMap    as Module
import qualified ModularPrelude.Module.List        as Module
import qualified ModularPrelude.Module.Set         as Module
import qualified ModularPrelude.Module.HashSet     as Module
import qualified ModularPrelude.Module.FilePath    as Module


import qualified Data.ByteString                   as ByteString
import qualified Data.ByteString.Lazy              as LByteString
import qualified Data.Text                         as Text
import qualified Data.Text.Lazy                    as LText
import qualified Data.Vector                       as Vector
import qualified Data.Vector.Unboxed               as UVector
import qualified Data.Map                          as Map
import qualified Data.HashMap.Strict               as HashMap
import qualified Data.HashMap.Lazy                 as LHashMap
import qualified Data.List                         as List
import qualified Data.Set                          as Set
import qualified Data.HashSet                      as HashSet
import qualified Filesystem.Path.CurrentOS         as FilePath



import           ModularPrelude.Classy
import qualified ModularPrelude.From.Classy        as From
import qualified ModularPrelude.Module.Classy      as Module

import qualified ClassyPrelude                     as Classy

