{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Void where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance IsSome a Void where
  to = absurd
  maybeFrom = const Nothing
