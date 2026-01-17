{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndWord8List where

import qualified Data.Primitive.ByteArray
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.ByteArrayAndShortByteString ()

instance IsSubsetOf Data.Primitive.ByteArray.ByteArray [Word8] where
  to = fromList

instance IsSubsetOf [Word8] Data.Primitive.ByteArray.ByteArray where
  to = toList

instance Is Data.Primitive.ByteArray.ByteArray [Word8]

instance Is [Word8] Data.Primitive.ByteArray.ByteArray
