{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndText where

import Data.Text (Text)
import qualified Data.Text.Lazy
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Data.Text.Lazy.Text Text where
  to = Data.Text.Lazy.fromStrict

instance NormalizesTo Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance Is Data.Text.Lazy.Text Text

instance Is Text Data.Text.Lazy.Text
