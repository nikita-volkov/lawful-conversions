{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndText where

import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome ByteString Text where
  to = Data.Text.Encoding.encodeUtf8
  maybeFrom = either (const Nothing) Just . Data.Text.Encoding.decodeUtf8'

instance IsInjectionOf ByteString Text where
  lenientlyFrom = Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
