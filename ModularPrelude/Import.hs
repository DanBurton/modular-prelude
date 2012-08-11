{-# LANGUAGE NoImplicitPrelude #-}

module ModularPrelude.Import where


import           ModularPrelude
import qualified ModularPrelude.From        as From


import qualified ModularPrelude.ByteString  as Import
import qualified ModularPrelude.LByteString as Import
import qualified ModularPrelude.Text        as Import
import qualified ModularPrelude.LText       as Import
import qualified ModularPrelude.Vector      as Import
import qualified ModularPrelude.UVector     as Import
import qualified ModularPrelude.Map         as Import
import qualified ModularPrelude.HashMap     as Import
import qualified ModularPrelude.LHashMap    as Import
import qualified ModularPrelude.List        as Import
import qualified ModularPrelude.Set         as Import
import qualified ModularPrelude.HashSet     as Import
import qualified ModularPrelude.FilePath    as Import
import qualified ModularPrelude.Classy      as Import


import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Lazy       as LByteString
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Vector                as Vector
import qualified Data.Vector.Unboxed        as UVector
import qualified Data.Map                   as Map
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashMap.Lazy          as LHashMap
import qualified Data.List                  as List
import qualified Data.Set                   as Set
import qualified Data.HashSet               as HashSet
import qualified Filesystem.Path.CurrentOS  as FilePath
import qualified ClassyPrelude              as Classy

