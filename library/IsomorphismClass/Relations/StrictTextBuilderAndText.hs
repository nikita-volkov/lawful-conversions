{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.StrictTextBuilderAndText where

#if MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Text Data.Text.Encoding.StrictBuilder where
  to = Data.Text.Encoding.strictBuilderToText

instance IsSome Data.Text.Encoding.StrictBuilder Text where
  to = Data.Text.Encoding.textToStrictBuilder

instance IsAll Text Data.Text.Encoding.StrictBuilder

instance IsAll Data.Text.Encoding.StrictBuilder Text

instance Is Text Data.Text.Encoding.StrictBuilder

instance Is Data.Text.Encoding.StrictBuilder Text

#endif
