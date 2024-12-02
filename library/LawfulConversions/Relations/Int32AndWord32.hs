{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int32AndWord32 where

import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Int32 Word32 where
  to = fromIntegral

instance IsSome Word32 Int32 where
  to = fromIntegral

instance IsAll Int32 Word32

instance IsAll Word32 Int32

instance Is Int32 Word32

instance Is Word32 Int32
