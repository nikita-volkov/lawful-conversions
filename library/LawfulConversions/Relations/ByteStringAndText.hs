{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndText where

import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome ByteString Text where
  to = Data.Text.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Encoding.decodeUtf8'

instance IsAll Text ByteString where
  onto = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
