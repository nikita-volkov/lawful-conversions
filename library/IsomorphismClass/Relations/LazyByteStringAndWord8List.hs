{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringAndWord8List where

import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Data.ByteString.Lazy.ByteString [Word8] where
  to = Data.ByteString.Lazy.pack

instance IsSome [Word8] Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.unpack

instance IsAll Data.ByteString.Lazy.ByteString [Word8]

instance IsAll [Word8] Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString [Word8]

instance Is [Word8] Data.ByteString.Lazy.ByteString
