{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndText where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Text Data.Text.Encoding.StrictTextBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance NormalizesTo Data.Text.Encoding.StrictTextBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance NormalizesTo Data.Text.Encoding.StrictBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

#endif
