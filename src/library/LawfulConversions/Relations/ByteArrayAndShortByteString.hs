{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndShortByteString where

import qualified Data.ByteString.Short
import qualified Data.ByteString.Short.Internal
import qualified Data.Primitive.ByteArray
import LawfulConversions.Algebra

instance IsSubsetOf Data.ByteString.Short.ShortByteString Data.Primitive.ByteArray.ByteArray where
  to (Data.Primitive.ByteArray.ByteArray array) =
    Data.ByteString.Short.Internal.SBS array

instance IsSubsetOf Data.Primitive.ByteArray.ByteArray Data.ByteString.Short.ShortByteString where
  to (Data.ByteString.Short.Internal.SBS array) =
    Data.Primitive.ByteArray.ByteArray array

instance Is Data.Primitive.ByteArray.ByteArray Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString Data.Primitive.ByteArray.ByteArray
