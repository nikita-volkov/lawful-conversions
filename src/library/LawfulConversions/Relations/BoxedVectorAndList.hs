{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.BoxedVectorAndList where

import Data.Vector (Vector)
import qualified Data.Vector
import LawfulConversions.Algebra

instance IsSubsetOf (Vector a) [a] where
  to = Data.Vector.fromList

instance IsSubsetOf [a] (Vector a) where
  to = Data.Vector.toList

instance Is (Vector a) [a]

instance Is [a] (Vector a)
