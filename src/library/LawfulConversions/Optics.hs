module LawfulConversions.Optics where

import Data.Profunctor
import LawfulConversions.Algebra
import LawfulConversions.Prelude

-- | Van-Laarhoven-style Prism, compatible with libraries like \"lens\" and \"optics\".
isSubsetOfPrism :: (IsSubsetOf a b, Choice p, Applicative f) => p b (f b) -> p a (f a)
isSubsetOfPrism =
  dimap
    (\s -> maybe (Left s) Right (maybeFrom s))
    (either pure (fmap to))
    . right'

-- | Van-Laarhoven-style Isomorphism, compatible with libraries like \"lens\" and \"optics\".
isSubsetOfIso :: (IsSubsetOf a b, Profunctor p, Functor f) => p b (f b) -> p a (f a)
isSubsetOfIso = dimap onfrom (fmap to)

-- | Van-Laarhoven-style Isomorphism, compatible with libraries like \"lens\" and \"optics\".
isIso :: (Is a b, Profunctor p, Functor f) => p b (f b) -> p a (f a)
isIso = isSubsetOfIso
