{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.LazyTextAndText where

import qualified Data.Text.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict

instance IsSome Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance SurjectsTo Data.Text.Lazy.Text Text

instance SurjectsTo Text Data.Text.Lazy.Text

instance Is Data.Text.Lazy.Text Text

instance Is Text Data.Text.Lazy.Text
