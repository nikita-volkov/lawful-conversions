module LawfulConversions.Classes.IsAll where

import LawfulConversions.Classes.IsSome

-- |
-- Lossy or canonicalizing conversion.
-- Captures mappings from multiple alternative inputs into one output.
--
-- E.g., ByteString can be decoded into Text with UTF-8 leniently, replacing the invalid chars.
--
-- `String` has a wider range of supported chars than `Text` and so there cannot be a one-to-one mapping covered by the `Is` class, however it is
--
-- === Laws
--
-- ==== 'onto' is an inverse of 'to'
--
-- > \a -> a == onto (to a)
class (IsSome b a) => IsAll a b where
  -- |
  -- Surjection from B to A.
  --
  -- Version of 'maybeFrom', which provides default handling of cases where 'maybeFrom' would have produced 'Nothing'.
  --
  -- In mathematics, a surjective function (also known as surjection, or onto function) is a function f such that, for every element y of the function's codomain, there exists at least one element x in the function's domain such that f(x) = y. In other words, for a function f : X → Y, the codomain Y is the image of the function's domain X. It is not required that x be unique; the function f may map one or more elements of X to the same element of Y.
  onto :: b -> a

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default onto :: (IsSome a b) => b -> a
  onto = to

instance IsAll a a
