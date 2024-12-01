{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndWord8List where

import qualified Data.ByteString
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome ByteString [Word8] where
  to = Data.ByteString.pack

instance IsSome [Word8] ByteString where
  to = Data.ByteString.unpack

instance IsAll ByteString [Word8]

instance IsAll [Word8] ByteString

instance Is ByteString [Word8]

instance Is [Word8] ByteString
