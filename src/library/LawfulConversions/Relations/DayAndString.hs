{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndString where

import Data.Time
import Data.Time.Format.ISO8601
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | Implements ISO-8601.
instance NormalizesTo String Day where
  to = iso8601Show
  maybeFrom = iso8601ParseM
  onfrom = fromMaybe (ModifiedJulianDay 0) . iso8601ParseM
