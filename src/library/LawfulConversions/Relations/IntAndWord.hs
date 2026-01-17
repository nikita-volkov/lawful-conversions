{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.IntAndWord where

import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSubsetOf Int Word where
  to = fromIntegral

instance IsSubsetOf Word Int where
  to = fromIntegral

instance Is Int Word

instance Is Word Int
