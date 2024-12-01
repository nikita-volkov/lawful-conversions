{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndShortByteString where

import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome ByteString Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.fromShort

instance IsSome Data.ByteString.Short.ShortByteString ByteString where
  to = Data.ByteString.Short.toShort

instance IsAll ByteString Data.ByteString.Short.ShortByteString

instance IsAll Data.ByteString.Short.ShortByteString ByteString

instance Is ByteString Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString ByteString
