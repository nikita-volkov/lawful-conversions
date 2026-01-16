{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndLazyText where

import qualified Data.Text.Lazy
import Data.Time
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.DayAndString ()
import LawfulConversions.Relations.LazyTextAndString ()

-- | Implements ISO-8601.
instance NormalizesTo Data.Text.Lazy.Text Day where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
  onfrom = fromMaybe (ModifiedJulianDay 0) . maybeFrom @String . to
