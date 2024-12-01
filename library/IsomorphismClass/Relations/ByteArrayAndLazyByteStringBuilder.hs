{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteArrayAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()
import IsomorphismClass.Relations.LazyByteStringBuilderAndShortByteString ()

instance IsSome Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsSome Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray where
  to = to . to @Data.ByteString.Short.ShortByteString

instance IsAll Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder

instance IsAll Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.Primitive.ByteArray.ByteArray
