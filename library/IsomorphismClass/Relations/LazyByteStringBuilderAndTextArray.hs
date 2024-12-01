{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyByteStringBuilderAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Short
import qualified Data.Text.Array
import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import qualified IsomorphismClass.TextCompat.Array

instance IsSome Data.ByteString.Builder.Builder Data.Text.Array.Array where
  to = Data.ByteString.Builder.shortByteString . IsomorphismClass.TextCompat.Array.toShortByteString

instance IsSome Data.Text.Array.Array Data.ByteString.Builder.Builder where
  to =
    IsomorphismClass.TextCompat.Array.fromShortByteString
      . Data.ByteString.Short.toShort
      . Data.ByteString.Lazy.toStrict
      . Data.ByteString.Builder.toLazyByteString

instance IsAll Data.Text.Array.Array Data.ByteString.Builder.Builder

instance IsAll Data.ByteString.Builder.Builder Data.Text.Array.Array

instance Is Data.Text.Array.Array Data.ByteString.Builder.Builder

instance Is Data.ByteString.Builder.Builder Data.Text.Array.Array

#endif
