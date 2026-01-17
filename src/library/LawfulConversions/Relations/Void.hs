{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Void where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance IsSupersetOf a Void where
  to = absurd
  maybeFrom = const Nothing
  onfrom _ = error "Cannot convert to Void: Void has no inhabitants"
