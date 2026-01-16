{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndUuid where

import qualified Data.Text.Lazy
import qualified Data.UUID.Types
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance NormalizesTo Data.Text.Lazy.Text UUID where
  to = Data.Text.Lazy.fromStrict . Data.UUID.Types.toText
  maybeFrom = Data.UUID.Types.fromText . Data.Text.Lazy.toStrict
  onfrom = fromMaybe Data.UUID.Types.nil . Data.UUID.Types.fromText . Data.Text.Lazy.toStrict
