{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndByteString where

import qualified Data.ByteString.Short
import qualified Data.Primitive.ByteArray
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()
import LawfulConversions.Relations.ByteStringAndShortByteString ()

instance NormalizesTo Data.Primitive.ByteArray.ByteArray ByteString where
  to = to @Data.Primitive.ByteArray.ByteArray . to @Data.ByteString.Short.ShortByteString
  onfrom = to @ByteString . to @Data.ByteString.Short.ShortByteString

instance NormalizesTo ByteString Data.Primitive.ByteArray.ByteArray where
  to = to @ByteString . to @Data.ByteString.Short.ShortByteString
  onfrom = to @Data.Primitive.ByteArray.ByteArray . to @Data.ByteString.Short.ShortByteString

instance Is Data.Primitive.ByteArray.ByteArray ByteString

instance Is ByteString Data.Primitive.ByteArray.ByteArray
