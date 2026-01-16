{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ListAndSeq where

import Data.Sequence (Seq)
import qualified Data.Sequence
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo [a] (Seq a) where
  to = toList

instance NormalizesTo (Seq a) [a] where
  to = Data.Sequence.fromList

instance Is [a] (Seq a)

instance Is (Seq a) [a]
