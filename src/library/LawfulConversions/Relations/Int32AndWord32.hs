{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int32AndWord32 where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf Int32 Word32 where
  to = fromIntegral

instance IsSupersetOf Word32 Int32 where
  to = fromIntegral

instance Is Int32 Word32

instance Is Word32 Int32
