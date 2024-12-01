{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.BoxedVectorAndList where

import qualified Data.Vector
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome (Vector a) [a] where
  to = Data.Vector.fromList

instance IsSome [a] (Vector a) where
  to = Data.Vector.toList

instance IsAll (Vector a) [a]

instance IsAll [a] (Vector a)

instance Is (Vector a) [a]

instance Is [a] (Vector a)
