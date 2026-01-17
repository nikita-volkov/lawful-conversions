{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndStrictTextBuilder where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSubsetOf Data.Text.Lazy.Text Data.Text.Encoding.StrictTextBuilder where
  to = Data.Text.Lazy.fromStrict . Data.Text.Encoding.strictBuilderToText

instance IsSubsetOf Data.Text.Encoding.StrictTextBuilder Data.Text.Lazy.Text where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSubsetOf Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Lazy.fromStrict . Data.Text.Encoding.strictBuilderToText

instance IsSubsetOf Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict

#endif
