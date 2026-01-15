{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Any where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | Any type is isomorphic to itself.
instance IsSome a a where
  to = id
  maybeFrom = Just . id

-- | Any type is isomorphic to itself.
instance IsMany a a

-- | Any type is isomorphic to itself.
instance Is a a
