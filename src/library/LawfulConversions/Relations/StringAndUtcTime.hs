{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StringAndUtcTime where

import Data.Time
import Data.Time.Format.ISO8601
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | Implements ISO-8601.
instance NormalizesTo String UTCTime where
  to = iso8601Show
  maybeFrom = iso8601ParseM
  onfrom = fromMaybe (UTCTime (ModifiedJulianDay 0) 0) . iso8601ParseM
