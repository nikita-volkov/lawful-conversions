{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ListAndSeq where

import qualified Data.Sequence
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome [a] (Seq a) where
  to = toList

instance IsSome (Seq a) [a] where
  to = Data.Sequence.fromList

instance IsAll [a] (Seq a)

instance IsAll (Seq a) [a]

instance Is [a] (Seq a)

instance Is (Seq a) [a]
