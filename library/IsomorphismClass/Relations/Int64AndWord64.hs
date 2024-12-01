{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int64AndWord64 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Int64 Word64 where
  to = fromIntegral

instance IsSome Word64 Int64 where
  to = fromIntegral

instance IsAll Int64 Word64

instance IsAll Word64 Int64

instance Is Int64 Word64

instance Is Word64 Int64
