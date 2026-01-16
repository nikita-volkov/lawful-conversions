{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndLazyByteStringBuilder where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo ByteString Data.ByteString.Builder.Builder where
  to = Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString

instance NormalizesTo Data.ByteString.Builder.Builder ByteString where
  to = Data.ByteString.Builder.byteString

instance Is Data.ByteString.Builder.Builder ByteString

instance Is ByteString Data.ByteString.Builder.Builder
