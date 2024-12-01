{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntMapAndMapOfInt where

import qualified Data.IntMap.Strict
import qualified Data.Map.Strict
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome (Map Int v) (IntMap v) where
  to = Data.Map.Strict.fromList . Data.IntMap.Strict.toList

instance IsSome (IntMap v) (Map Int v) where
  to = Data.IntMap.Strict.fromList . Data.Map.Strict.toList

instance IsAll (Map Int v) (IntMap v)

instance IsAll (IntMap v) (Map Int v)

instance Is (Map Int v) (IntMap v)

instance Is (IntMap v) (Map Int v)
