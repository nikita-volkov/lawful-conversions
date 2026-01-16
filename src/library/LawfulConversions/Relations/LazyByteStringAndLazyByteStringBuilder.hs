{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndLazyByteStringBuilder where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import LawfulConversions.Algebra

instance NormalizesTo Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Builder.toLazyByteString

instance NormalizesTo Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Builder.lazyByteString

instance Is Data.ByteString.Lazy.ByteString Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.ByteString.Lazy.ByteString
