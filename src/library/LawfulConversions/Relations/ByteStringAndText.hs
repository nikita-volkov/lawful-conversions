{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndText where

import Data.ByteString (ByteString)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | UTF-8 codec.
instance IsSupersetOf ByteString Data.Text.Text where
  to = Data.Text.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Encoding.decodeUtf8'
  onfrom = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
