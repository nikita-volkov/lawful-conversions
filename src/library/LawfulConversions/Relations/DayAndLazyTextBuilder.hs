{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndLazyTextBuilder where

import qualified Data.Text.Lazy.Builder
import Data.Time
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.DayAndString ()
import LawfulConversions.Relations.LazyTextBuilderAndString ()

-- | Implements ISO-8601.
instance NormalizesTo Data.Text.Lazy.Builder.Builder Day where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
  onfrom = fromMaybe (ModifiedJulianDay 0) . maybeFrom @String . to
