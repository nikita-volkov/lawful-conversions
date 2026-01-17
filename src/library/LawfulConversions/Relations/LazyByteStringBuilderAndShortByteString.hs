{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringBuilderAndShortByteString where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Builder.shortByteString

instance IsSupersetOf Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString
  maybeFrom = Just . Data.ByteString.Builder.shortByteString

instance Is Data.ByteString.Builder.Builder Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString Data.ByteString.Builder.Builder
