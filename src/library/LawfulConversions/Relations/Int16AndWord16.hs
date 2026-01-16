{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int16AndWord16 where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Int16 Word16 where
  to = fromIntegral

instance NormalizesTo Word16 Int16 where
  to = fromIntegral

instance Is Int16 Word16

instance Is Word16 Int16
