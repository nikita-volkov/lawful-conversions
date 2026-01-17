{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndWord8List where

import Data.ByteString (ByteString)
import qualified Data.ByteString
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSubsetOf ByteString [Word8] where
  to = Data.ByteString.pack

instance IsSubsetOf [Word8] ByteString where
  to = Data.ByteString.unpack

instance Is ByteString [Word8]

instance Is [Word8] ByteString
