module IsomorphismClass.Classes.IsInjectionOf where

import IsomorphismClass.Classes.IsSome

-- |
-- In mathematics, a surjective function (also known as surjection, or onto function) is a function f such that, for every element y of the function's codomain, there exists at least one element x in the function's domain such that f(x) = y. In other words, for a function f : X → Y, the codomain Y is the image of the function's domain X. It is not required that x be unique; the function f may map one or more elements of X to the same element of Y.
class (IsSome a b) => IsInjectionOf a b where
  -- |
  -- Surjection from A to B.
  --
  -- Version of 'maybeFrom', which provides default handling of cases where 'maybeFrom' would have produced 'Nothing'.
  lenientlyFrom :: a -> b

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default lenientlyFrom :: (IsSome b a) => a -> b
  lenientlyFrom = to
