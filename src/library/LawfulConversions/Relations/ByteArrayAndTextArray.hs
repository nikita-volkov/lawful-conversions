{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteArrayAndTextArray where

#if !MIN_VERSION_text(2,1,0)

import qualified Data.Primitive.ByteArray
import qualified Data.Text.Array
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import qualified LawfulConversions.TextCompat.Array

instance NormalizesTo Data.Primitive.ByteArray.ByteArray Data.Text.Array.Array where
  to = LawfulConversions.TextCompat.Array.toByteArray

instance NormalizesTo Data.Text.Array.Array Data.Primitive.ByteArray.ByteArray where
  to = LawfulConversions.TextCompat.Array.fromByteArray

instance Is Data.Primitive.ByteArray.ByteArray Data.Text.Array.Array

instance Is Data.Text.Array.Array Data.Primitive.ByteArray.ByteArray

#endif
