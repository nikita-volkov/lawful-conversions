{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndText where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

instance Is Text Data.Text.Encoding.StrictBuilder

instance Is Data.Text.Encoding.StrictBuilder Text

#endif
