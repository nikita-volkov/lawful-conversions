module LawfulConversions.Optics where

import Data.Profunctor
import LawfulConversions.Classes
import LawfulConversions.Prelude

-- | Van-Laarhoven-style Prism, compatible with the \"lens\" library.
isSomePrism :: (IsSome a b, Choice p, Applicative f) => p b (f b) -> p a (f a)
isSomePrism =
  dimap
    (\s -> maybe (Left s) Right (maybeFrom s))
    (either pure (fmap to))
    . right'

-- | Van-Laarhoven-style Isomorphism, compatible with the \"lens\" library.
isManyIso :: (IsMany a b, Profunctor p, Functor f) => p b (f b) -> p a (f a)
isManyIso = dimap from (fmap to)

-- | Van-Laarhoven-style Isomorphism, compatible with the \"lens\" library.
isIso :: (Is a b, Profunctor p, Functor f) => p b (f b) -> p a (f a)
isIso = isManyIso