{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ByteStringAndLazyByteString where

import qualified Data.ByteString.Lazy
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsSome ByteString Data.ByteString.Lazy.ByteString where
  to = Data.ByteString.Lazy.toStrict

instance IsSome Data.ByteString.Lazy.ByteString ByteString where
  to = Data.ByteString.Lazy.fromStrict

instance IsAll ByteString Data.ByteString.Lazy.ByteString

instance IsAll Data.ByteString.Lazy.ByteString ByteString

instance Is ByteString Data.ByteString.Lazy.ByteString

instance Is Data.ByteString.Lazy.ByteString ByteString
