{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndText where

import qualified Data.Text
import qualified Data.Text.Lazy
import LawfulConversions.Algebra

instance IsSubsetOf Data.Text.Lazy.Text Data.Text.Text where
  to = Data.Text.Lazy.fromStrict

instance IsSubsetOf Data.Text.Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance Is Data.Text.Lazy.Text Data.Text.Text

instance Is Data.Text.Text Data.Text.Lazy.Text
