{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntMapAndMapOfInt where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf (Map Int v) (IntMap v) where
  to = Data.Map.Strict.fromList . Data.IntMap.Strict.toList

instance IsSupersetOf (IntMap v) (Map Int v) where
  to = Data.IntMap.Strict.fromList . Data.Map.Strict.toList

instance Is (Map Int v) (IntMap v)

instance Is (IntMap v) (Map Int v)
