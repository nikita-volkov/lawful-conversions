{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndLazyByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()
import IsomorphismClass.Relations.LazyByteStringAndShortByteString ()

instance IsSome Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsSome Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsAll Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString

instance IsAll Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray
