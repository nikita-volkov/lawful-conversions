{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndLazyText where

import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | UTF-8 codec.
instance NormalizesTo ByteString Data.Text.Lazy.Text where
  to = Data.ByteString.Lazy.toStrict . Data.Text.Lazy.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Lazy.Encoding.decodeUtf8' . Data.ByteString.Lazy.fromStrict
  onfrom = Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode . Data.ByteString.Lazy.fromStrict
