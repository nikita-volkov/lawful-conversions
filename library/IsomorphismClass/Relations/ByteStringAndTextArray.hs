{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance IsSome ByteString Data.Text.Array.Array where
  to = Data.ByteString.Short.fromShort . IsomorphismClass.TextCompat.Array.toShortByteString

instance IsSome Data.Text.Array.Array ByteString where
  to = IsomorphismClass.TextCompat.Array.fromShortByteString . Data.ByteString.Short.toShort

instance IsAll ByteString Data.Text.Array.Array

instance IsAll Data.Text.Array.Array ByteString

instance Is ByteString Data.Text.Array.Array

instance Is Data.Text.Array.Array ByteString

#endif
