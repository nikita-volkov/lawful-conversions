{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntMapAndMapOfInt where

import qualified Data.IntMap.Strict
import qualified Data.Map.Strict
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo (Map Int v) (IntMap v) where
  to = Data.Map.Strict.fromList . Data.IntMap.Strict.toList

instance NormalizesTo (IntMap v) (Map Int v) where
  to = Data.IntMap.Strict.fromList . Data.Map.Strict.toList

instance Is (Map Int v) (IntMap v)

instance Is (IntMap v) (Map Int v)
