{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyByteStringAndLazyText where

import qualified Data.ByteString.Lazy
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | UTF-8 codec.
instance IsSubsetOf Data.ByteString.Lazy.ByteString Data.Text.Lazy.Text where
  to = Data.Text.Lazy.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Lazy.Encoding.decodeUtf8'
  onfrom = Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
