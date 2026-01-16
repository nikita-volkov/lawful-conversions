{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.TextAndUtcTime where

import Data.Time
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Relations.StringAndUtcTime ()

-- | Implements ISO-8601.
instance NormalizesTo Text UTCTime where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
  onfrom = fromMaybe (UTCTime (ModifiedJulianDay 0) 0) . maybeFrom @String . to
