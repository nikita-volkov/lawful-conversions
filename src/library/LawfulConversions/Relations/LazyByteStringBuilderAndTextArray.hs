{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringBuilderAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance IsSupersetOf Data.ByteString.Builder.Builder Data.Text.Array.Array where
  to = Data.ByteString.Builder.shortByteString . LawfulConversions.TextCompat.Array.toShortByteString

instance IsSupersetOf Data.Text.Array.Array Data.ByteString.Builder.Builder where
  to =
    LawfulConversions.TextCompat.Array.fromShortByteString
      . Data.ByteString.Short.toShort
      . Data.ByteString.Lazy.toStrict
      . Data.ByteString.Builder.toLazyByteString

instance Is Data.Text.Array.Array Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.Text.Array.Array

#endif
