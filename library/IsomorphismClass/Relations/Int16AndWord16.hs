{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int16AndWord16 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Int16 Word16 where
  to = fromIntegral

instance IsSome Word16 Int16 where
  to = fromIntegral

instance IsAll Int16 Word16

instance IsAll Word16 Int16

instance Is Int16 Word16

instance Is Word16 Int16
