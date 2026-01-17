{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndText where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import LawfulConversions.Algebra
import qualified Data.Text

instance IsSupersetOf Data.Text.Text Data.Text.Encoding.StrictTextBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsSupersetOf Data.Text.Encoding.StrictTextBuilder Data.Text.Text where
  to = Data.Text.Encoding.textToStrictBuilder

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import LawfulConversions.Algebra
import qualified Data.Text

instance IsSupersetOf Data.Text.Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsSupersetOf Data.Text.Encoding.StrictBuilder Data.Text.Text where
  to = Data.Text.Encoding.textToStrictBuilder

#endif
