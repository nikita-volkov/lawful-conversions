{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndShortByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short
import LawfulConversions.Algebra

instance IsSupersetOf ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort

instance IsSupersetOf Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort

instance Is ByteString Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString ByteString
