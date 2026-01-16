{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StringAndUuid where

import qualified Data.UUID.Types
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo String UUID where
  to = Data.UUID.Types.toString
  maybeFrom = Data.UUID.Types.fromString
  onfrom = fromMaybe Data.UUID.Types.nil . Data.UUID.Types.fromString
