{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndLazyByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()
import LawfulConversions.Relations.LazyByteStringAndShortByteString ()

instance NormalizesTo Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance NormalizesTo Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString where
  to = to . to @Data.ByteString.Short.ShortByteString

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString Data.Primitive.ByteArray.ByteArray
