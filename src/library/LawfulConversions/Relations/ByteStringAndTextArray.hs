{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSubsetOf ByteString Data.Text.Array.Array where
  to = Data.ByteString.Short.fromShort . LawfulConversions.TextCompat.Array.toShortByteString

instance IsSubsetOf Data.Text.Array.Array ByteString where
  to = LawfulConversions.TextCompat.Array.fromShortByteString . Data.ByteString.Short.toShort

instance Is ByteString Data.Text.Array.Array

instance Is Data.Text.Array.Array ByteString

#endif
