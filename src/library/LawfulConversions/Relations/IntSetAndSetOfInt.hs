{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntSetAndSetOfInt where

import Data.IntSet (IntSet)
import Data.Set (Set)
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf (Set Int) IntSet where
  to = fromList . toList

instance IsSupersetOf IntSet (Set Int) where
  to = fromList . toList

instance Is (Set Int) IntSet

instance Is IntSet (Set Int)
