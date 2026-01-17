{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()
import LawfulConversions.Relations.LazyByteStringBuilderAndShortByteString ()

instance IsSupersetOf Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsSupersetOf Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray
