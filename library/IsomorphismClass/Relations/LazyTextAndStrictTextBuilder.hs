{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndStrictTextBuilder where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Lazy.fromStrict . Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text where
  to = Data.Text.Encoding.textToStrictBuilder . Data.Text.Lazy.toStrict

instance IsAll Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder

instance IsAll Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text

instance Is Data.Text.Lazy.Text Data.Text.Encoding.StrictBuilder

instance Is Data.Text.Encoding.StrictBuilder Data.Text.Lazy.Text

#endif
