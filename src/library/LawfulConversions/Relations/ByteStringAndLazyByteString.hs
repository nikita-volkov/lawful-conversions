{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ByteStringAndLazyByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import LawfulConversions.Algebra
import LawfulConversions.Prelude

instance IsSupersetOf ByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.toStrict

instance IsSupersetOf Data.ByteString.Lazy.ByteString ByteString where
  to = Data.ByteString.Lazy.fromStrict

instance Is ByteString Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString ByteString
