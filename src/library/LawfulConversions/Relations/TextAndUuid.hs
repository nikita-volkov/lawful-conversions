{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.TextAndUuid where

import qualified Data.UUID.Types as Uuid
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Text UUID where
  to = Uuid.toText
  maybeFrom = Uuid.fromText
  onfrom = fromMaybe Uuid.nil . Uuid.fromText
