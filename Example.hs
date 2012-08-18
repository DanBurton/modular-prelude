{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ModularPrelude
import qualified ModularPrelude.From          as From

import qualified ModularPrelude.Module.Text   as Module
import qualified ModularPrelude.Module.List   as Module
import qualified ModularPrelude.Module.Classy as Module


import Data.Char (isSpace)

-- a really dumb example.
-- TODO: make more interesting examples
--       that leverage more of this entire ecosystem

main = do
  putStrLn $ dropWhile isSpace $ "   Hello, world"
  putStrLn $ show $ ldropWhile odd [1, 2, 3]
  where
    Module.Text{pack, dropWhile} = def
    Module.List{dropWhile=ldropWhile} = def
    Module.Classy{show} = def

