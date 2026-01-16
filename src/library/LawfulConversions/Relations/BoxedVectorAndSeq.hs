{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.BoxedVectorAndSeq where

import qualified Data.Sequence
import qualified Data.Vector
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo (Vector a) (Seq a) where
  to = Data.Vector.fromList . toList

instance NormalizesTo (Seq a) (Vector a) where
  to = fromList . Data.Vector.toList

instance Is (Vector a) (Seq a)

instance Is (Seq a) (Vector a)
