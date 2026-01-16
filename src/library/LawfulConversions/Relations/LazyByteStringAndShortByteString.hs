{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndShortByteString where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Lazy.fromStrict . Data.ByteString.Short.fromShort

instance NormalizesTo Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Short.toShort . Data.ByteString.Lazy.toStrict

instance Is Data.ByteString.Lazy.ByteString Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString Data.ByteString.Lazy.ByteString
