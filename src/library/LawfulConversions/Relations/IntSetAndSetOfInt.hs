{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntSetAndSetOfInt where

import Data.IntSet (IntSet)
import Data.Set (Set)
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSubsetOf (Set Int) IntSet where
  to = fromList . toList

instance IsSubsetOf IntSet (Set Int) where
  to = fromList . toList

instance Is (Set Int) IntSet

instance Is IntSet (Set Int)
