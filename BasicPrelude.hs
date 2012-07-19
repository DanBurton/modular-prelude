{-# LANGUAGE NoImplicitPrelude #-}
module BasicPrelude
    ( -- * Standard
      -- ** Operators
      (Prelude.$)
    , (Prelude.+)
    , (Prelude.-)
    , (Prelude.*)
    , (Prelude./)
    , (Prelude.&&)
    , (Prelude.||)
    , (Prelude..)
      -- ** Functions
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Prelude.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.error
    , Prelude.zip
    , Prelude.unzip
    , Prelude.zipWith
    , Prelude.or
    , Data.Text.IO.putStrLn
    , Prelude.elem
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
      -- ** Type classes
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Enum (..)
    , Prelude.Show
    , Prelude.Functor (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
      -- ** Data types
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.IO
    , Prelude.Either (..)
    , Prelude.Integral (..)
      -- * Re-exports
      -- ** Packed reps
    , ByteString
    , LByteString
    , Text
    , LText
      -- ** Containers
    , Map
    , HashMap
    , LHashMap
    , Set
    , HashSet
    , Vector
    , Hashable
      -- ** Numbers
    , Word8
    , Word64
    , Int64
    , Prelude.Int
    , Word
      -- ** Monoids
    , Monoid (..)
    , concat
    , (++)
      -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
      -- ** Either
    , Data.Either.partitionEithers
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
      -- ** Monad
    , (Control.Monad.>=>)
      -- ** Transformers
    , Control.Monad.Trans.Class.lift
    , Control.Monad.IO.Class.MonadIO
    , Control.Monad.IO.Class.liftIO
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Control.Exception.SomeException
    , Control.Exception.throwIO
      -- ** Files
    , F.FilePath
    , (F.</>)
    , (F.<.>)
    , F.hasExtension
    , F.basename
    , F.filename
      -- ** Print
    , Prelude.print
    ) where

import qualified Prelude
import Prelude (Char, (.))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Lazy as HML (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)

import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import qualified Control.Applicative
import qualified Control.Monad
import qualified Control.Exception

import qualified Filesystem.Path.CurrentOS as F

import Data.Word (Word8, Word64, Word)
import Data.Int (Int64)

import qualified Data.Text.IO

import qualified Data.Maybe
import qualified Data.Either

import qualified Control.Monad.Trans.Class
import qualified Control.Monad.IO.Class

type LByteString = L.ByteString
type LText = TL.Text
type LHashMap = HML.HashMap


concat :: Monoid w => [w] -> w
concat = mconcat

infixr 5  ++
(++) :: Monoid w => w -> w -> w
(++) = mappend
