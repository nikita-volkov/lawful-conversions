{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ShortByteStringAndWord8List where

import qualified Data.ByteString.Short
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import IsomorphismClass.Relations.ByteArrayAndShortByteString ()

instance IsSome [Word8] Data.ByteString.Short.ShortByteString where
  to = Data.ByteString.Short.unpack

instance IsSome Data.ByteString.Short.ShortByteString [Word8] where
  to = Data.ByteString.Short.pack

instance IsAll [Word8] Data.ByteString.Short.ShortByteString

instance IsAll Data.ByteString.Short.ShortByteString [Word8]

instance Is [Word8] Data.ByteString.Short.ShortByteString

instance Is Data.ByteString.Short.ShortByteString [Word8]
