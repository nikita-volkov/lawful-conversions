{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.BoxedVectorAndSeq where

import Data.Sequence (Seq)
import qualified Data.Sequence
import Data.Vector (Vector)
import qualified Data.Vector
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf (Vector a) (Seq a) where
  to = Data.Vector.fromList . toList

instance IsSupersetOf (Seq a) (Vector a) where
  to = fromList . Data.Vector.toList

instance Is (Vector a) (Seq a)

instance Is (Seq a) (Vector a)
