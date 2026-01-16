{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextBuilderAndUtcTime where

import qualified Data.Text.Lazy.Builder
import Data.Time
import LawfulConversions.Algebra
import LawfulConversions.Prelude
import LawfulConversions.Relations.LazyTextBuilderAndString ()
import LawfulConversions.Relations.StringAndUtcTime ()

-- | Implements ISO-8601.
instance NormalizesTo Data.Text.Lazy.Builder.Builder UTCTime where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
  onfrom = fromMaybe (UTCTime (ModifiedJulianDay 0) 0) . maybeFrom @String . to
