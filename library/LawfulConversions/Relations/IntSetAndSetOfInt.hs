{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntSetAndSetOfInt where

import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome (Set Int) IntSet where
  to = fromList . toList

instance IsSome IntSet (Set Int) where
  to = fromList . toList

instance IsAll (Set Int) IntSet

instance IsAll IntSet (Set Int)

instance Is (Set Int) IntSet

instance Is IntSet (Set Int)
