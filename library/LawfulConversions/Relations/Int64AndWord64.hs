{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int64AndWord64 where

import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Int64 Word64 where
  to = fromIntegral

instance IsSome Word64 Int64 where
  to = fromIntegral

instance Is Int64 Word64

instance Is Word64 Int64
