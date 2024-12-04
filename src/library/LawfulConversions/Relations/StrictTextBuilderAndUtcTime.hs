{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndUtcTime where

import qualified Data.Text.Encoding
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.StrictTextBuilderAndString ()
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Relations.StringAndUtcTime ()

#if MIN_VERSION_text(2,1,2)

instance IsSome Data.Text.Encoding.StrictTextBuilder UTCTime where
  to = from . to @String
  maybeFrom = maybeFrom @String . to

#else
#if MIN_VERSION_text(2,0,2)

instance IsSome Data.Text.Encoding.StrictBuilder UTCTime where
  to = from . to @String
  maybeFrom = maybeFrom @String . to

#endif
#endif
