{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndShortByteString where

import qualified Data.ByteString.Short
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort

instance NormalizesTo Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort

instance Is ByteString Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString ByteString
