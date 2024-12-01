module IsomorphismClass.Classes.IsInjectionOf where

-- |
-- In mathematics, a surjective function (also known as surjection, or onto function) is a function f such that, for every element y of the function's codomain, there exists at least one element x in the function's domain such that f(x) = y. In other words, for a function f : X → Y, the codomain Y is the image of the function's domain X. It is not required that x be unique; the function f may map one or more elements of X to the same element of Y.
class IsInjectionOf a b where
  -- |
  -- Surjection from A to B.
  --
  -- This function may lose information.
  onto :: a -> b

  -- |
  -- Injection from B to A.
  --
  -- An inverse of 'onto', which chooses the preferable default.
  into :: b -> a
