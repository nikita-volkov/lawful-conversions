{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.Int8AndWord8 where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf Int8 Word8 where
  to = fromIntegral

instance IsSupersetOf Word8 Int8 where
  to = fromIntegral

instance Is Int8 Word8

instance Is Word8 Int8
