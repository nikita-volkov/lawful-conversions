{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndWord8List where

import qualified Data.ByteString.Lazy
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Data.ByteString.Lazy.ByteString [Word8] where
  to = Data.ByteString.Lazy.pack

instance NormalizesTo [Word8] Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.unpack

instance Is Data.ByteString.Lazy.ByteString [Word8]

instance Is [Word8] Data.ByteString.Lazy.ByteString
