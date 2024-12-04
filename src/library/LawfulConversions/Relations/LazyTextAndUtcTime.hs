{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndUtcTime where

import qualified Data.Text.Lazy
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.LazyTextAndString ()
import LawfulConversions.Relations.StringAndUtcTime ()

instance IsSome Data.Text.Lazy.Text UTCTime where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
