module LawfulConversions.Classes.IsSome where

import LawfulConversions.Prelude

-- |
-- Evidence that all values of type @b@ form a subset of all values of type @a@.
--
-- [From Wikipedia](https://en.wikipedia.org/wiki/Subset):
--
-- In mathematics, a set A is a subset of a set B if all elements of A are also elements of B; B is then a superset of A. It is possible for A and B to be equal; if they are unequal, then A is a proper subset of B. The relationship of one set being a subset of another is called inclusion (or sometimes containment). A is a subset of B may also be expressed as B includes (or contains) A or A is included (or contained) in B. A k-subset is a subset with k elements.
--
-- === Laws
--
-- ==== 'to' is [injective](https://en.wikipedia.org/wiki/Injective_function)
--
-- For every two values of type @b@ that are not equal converting with 'to' produces values that are not equal as well:
--
-- > \(b1, b2) -> b1 == b2 || to @a b1 /= to @a b2
--
-- ==== 'maybeFrom' is a [partial inverse](https://en.wikipedia.org/wiki/Inverse_function#Partial_inverses) of 'to'
--
-- For all values of @b@ converting to @a@ and then attempting to convert back to @b@ always succeeds and produces a value that is equal to the original:
--
-- > \b -> maybeFrom (to @a b) == Just b
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isSomeProperties'.
class IsSome a b where
  -- |
  -- Convert a value of a subset type to a superset type.
  to :: b -> a

  -- |
  -- [Partial inverse](https://en.wikipedia.org/wiki/Inverse_function#Partial_inverses) of 'to'.
  maybeFrom :: a -> Maybe b

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default maybeFrom :: (IsSome b a) => a -> Maybe b
  maybeFrom = Just . to

-- | Every type is isomorphic to itself.
instance IsSome a a where
  to = id
  maybeFrom = Just . id

-- | The empty set has no elements, and therefore is vacuously a subset of any set.
instance IsSome a Void where
  to = absurd
  maybeFrom = const Nothing
