{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndText where

import qualified Data.Text
import qualified Data.Text.Lazy
import LawfulConversions.Algebra

instance IsSupersetOf Data.Text.Lazy.Text Data.Text.Text where
  to = Data.Text.Lazy.fromStrict

instance IsSupersetOf Data.Text.Text Data.Text.Lazy.Text where
  to = Data.Text.Lazy.toStrict

instance Is Data.Text.Lazy.Text Data.Text.Text

instance Is Data.Text.Text Data.Text.Lazy.Text
