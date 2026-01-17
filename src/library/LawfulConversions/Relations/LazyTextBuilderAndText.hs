{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextBuilderAndText where

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSubsetOf Data.Text.Lazy.Builder.Builder Data.Text.Text where
  to = Data.Text.Lazy.Builder.fromText
  maybeFrom = Just . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText

instance IsSubsetOf Data.Text.Text Data.Text.Lazy.Builder.Builder where
  to = Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
  maybeFrom = Just . Data.Text.Lazy.Builder.fromText

instance Is Data.Text.Lazy.Builder.Builder Data.Text.Text

instance Is Data.Text.Text Data.Text.Lazy.Builder.Builder
