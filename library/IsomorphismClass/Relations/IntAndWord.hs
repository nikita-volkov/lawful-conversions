{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntAndWord where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome Int Word where
  to = fromIntegral

instance IsSome Word Int where
  to = fromIntegral

instance IsAll Int Word

instance IsAll Word Int

instance Is Int Word

instance Is Word Int
