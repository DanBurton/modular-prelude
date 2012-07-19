{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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


import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV
import qualified Data.Map                   as Map
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashMap.Lazy          as LHashMap
import qualified Data.List                  as List
import qualified Data.Set                   as Set
import qualified Data.HashSet               as HashSet
import qualified Filesystem.Path.CurrentOS  as F
import qualified ClassyPrelude              as Classy


import Data.Char (isSpace)
import qualified Prelude

-- a really dumb example.
-- TODO: make more interesting examples
--       that leverage more of this entire ecosystem

main = do
  putStrLn $ dropWhile isSpace $ "   Hello, world"
  putStrLn $ pack $ Prelude.show $ ldropWhile odd [1, 2, 3]
  where Import.T{pack, dropWhile} = def
        Import.List{dropWhile=ldropWhile} = def
