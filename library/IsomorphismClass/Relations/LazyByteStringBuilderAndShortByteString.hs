{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringBuilderAndShortByteString where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance PartiallyIsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Builder.shortByteString

instance PartiallyIsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString
  maybeFrom = Just . Data.ByteString.Builder.shortByteString

instance IsomorphicTo Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString

instance IsomorphicTo Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder
