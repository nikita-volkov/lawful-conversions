{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndText where

import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | UTF-8 codec.
instance IsSome ByteString Text where
  to = Data.Text.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Encoding.decodeUtf8'

-- | Lenient UTF-8 decoding.
instance IsMany ByteString Text where
  onfrom = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
