{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.BoxedVectorAndList where

import Data.Vector (Vector)
import qualified Data.Vector
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo (Vector a) [a] where
  to = Data.Vector.fromList

instance NormalizesTo [a] (Vector a) where
  to = Data.Vector.toList

instance Is (Vector a) [a]

instance Is [a] (Vector a)
