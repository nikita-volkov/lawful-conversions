{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextBuilderAndStrictTextBuilder where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf Data.Text.Lazy.Builder.Builder Data.Text.Encoding.StrictTextBuilder where
  to = Data.Text.Lazy.Builder.fromText . Data.Text.Encoding.strictBuilderToText

instance IsSupersetOf Data.Text.Encoding.StrictTextBuilder Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf Data.Text.Lazy.Builder.Builder Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Lazy.Builder.fromText . Data.Text.Encoding.strictBuilderToText

instance IsSupersetOf Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText

#endif
