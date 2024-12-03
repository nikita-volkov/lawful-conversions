module LawfulConversions.Classes.IsSome where

import LawfulConversions.Prelude

-- |
-- Evidence that all values of type @sub@ form a subset of all values of type @sup@.
--
-- [From Wikipedia](https://en.wikipedia.org/wiki/Subset):
--
-- In mathematics, a set A is a subset of a set B if all elements of A are also elements of B; B is then a superset of A. It is possible for A and B to be equal; if they are unequal, then A is a proper subset of B. The relationship of one set being a subset of another is called inclusion (or sometimes containment). A is a subset of B may also be expressed as B includes (or contains) A or A is included (or contained) in B. A k-subset is a subset with k elements.
--
-- === Laws
--
-- ==== 'to' is injective
--
-- For every two values of type @sub@ that are not equal converting with 'to' will always produce values that are not equal.
--
-- > \(a, b) -> a == b || to a /= to b
--
-- ==== 'maybeFrom' is an inverse of 'to'
--
-- For all values of @sub@ converting to @sup@ and then attempting to convert back to @sub@ always succeeds and produces a value that is equal to the original.
--
-- > \a -> maybeFrom (to a) == Just a
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isSomeProperties'.
class IsSome sup sub where
  -- |
  -- Convert a value of a subset type to a superset type.
  --
  -- This function is injective non-surjective.
  to :: sub -> sup

  -- |
  -- [Partial inverse](https://en.wikipedia.org/wiki/Inverse_function#Partial_inverses) of 'to'.
  maybeFrom :: sup -> Maybe sub

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default maybeFrom :: (IsSome sub sup) => sup -> Maybe sub
  maybeFrom = Just . to

-- | Every type is isomorphic to itself.
instance IsSome a a where
  to = id
  maybeFrom = Just . id

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance IsSome sup Void where
  to = absurd
  maybeFrom = const Nothing
