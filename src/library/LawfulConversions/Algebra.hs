module LawfulConversions.Algebra where

import LawfulConversions.Prelude

-- |
-- Evidence that type @b@ normalizes to type @a@. This captures bidirectional conversion patterns where:
-- - Type @b@ can be injected into type @a@ (subset relationship)
-- - Type @a@ can be canonicalized/normalized to type @b@ (possibly lossy)
--
-- This typeclass merges the concepts of subset inclusion and canonicalization, providing
-- both a total injection ('to') with its partial inverse ('maybeFrom'), and a total
-- (possibly lossy) surjection ('onfrom').
--
-- [From Wikipedia on Subsets](https://en.wikipedia.org/wiki/Subset):
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
-- ==== 'maybeFrom' characterizes the image of 'to'
--
-- 'maybeFrom' succeeds exactly on values that are in the image of 'to':
--
-- > \a b -> maybeFrom a == Just b ==> to b == a
--
-- ==== 'onfrom' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'to'
--
-- > \b -> b == onfrom (to @a b)
--
-- ==== 'onfrom' is [surjective](https://en.wikipedia.org/wiki/Surjective_function)
--
-- Every value of type @b@ can be obtained by applying 'onfrom' to some value of type @a@:
--
-- > \b -> exists a. onfrom @b a == b
--
-- Note: This property cannot be directly tested with QuickCheck as it requires existential quantification.
--
-- ==== Mathematical foundation
--
-- These laws establish that:
-- - Type @b@ forms a subset of type @a@ (via 'to' and 'maybeFrom')
-- - Type @a@ normalizes/canonicalizes to type @b@ (via 'onfrom')
-- The 'to' function provides the canonical injection, 'maybeFrom' recognizes which values of @a@
-- correspond to values from the subset @b@, and 'onfrom' provides a canonical (possibly lossy)
-- mapping from @a@ to @b@.
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.normalizesToProperties'.
class NormalizesTo a b where
  -- |
  -- Convert a value of a subset type to a superset type.
  to :: b -> a

  -- |
  -- [Partial inverse](https://en.wikipedia.org/wiki/Inverse_function#Partial_inverses) of 'to'.
  maybeFrom :: a -> Maybe b

  -- |
  -- Requires the presence of 'NormalizesTo' in reverse direction.
  default maybeFrom :: (NormalizesTo b a) => a -> Maybe b
  maybeFrom = Just . to

  -- |
  -- Possibly lossy inverse of 'to'.
  -- [Surjection](https://en.wikipedia.org/wiki/Surjective_function) from @a@ to @b@.
  --
  -- Particularly useful in combination with the @TypeApplications@ extension,
  -- where it allows to specify the input type, e.g.:
  --
  -- > fromString :: NormalizesTo String b => String -> b
  -- > fromString = onfrom @String
  --
  -- If you want to specify the output type instead, use 'onto'.
  onfrom :: a -> b

  -- |
  -- Requires the presence of 'NormalizesTo' in reverse direction.
  default onfrom :: (NormalizesTo b a) => a -> b
  onfrom = to

-- |
-- Convert a value of a superset type to a subset type specifying the superset type first.
--
-- Alias to 'to' with the only difference in the argument order.
--
-- E.g.,
--
-- > fromText = from @Text
from :: forall b a. (NormalizesTo a b) => b -> a
from = to

-- |
-- Try to convert a value of a superset type to a subset type specifying the target subset type first.
--
-- Alias to 'maybeFrom' with the only difference in the argument order.
--
-- Particularly useful in combination with the @TypeApplications@ extension,
-- where it allows to specify the target type, e.g.:
--
-- > maybeToInt16 :: Int32 -> Maybe Int16
-- > maybeToInt16 = maybeTo @Int16
--
-- > percent = maybeTo @Percent someDouble
maybeTo :: forall b a. (NormalizesTo a b) => a -> Maybe b
maybeTo = maybeFrom

-- |
-- Alias to 'onfrom', which lets you specify the target type of the conversion first using @TypeApplications@.
--
-- In mathematics @onto@ is another name for [Surjective function](https://en.wikipedia.org/wiki/Surjective_function).
--
-- E.g.,
--
-- > lenientDecodeUtf8 = onto @Text
--
-- @
-- combineTexts :: 'Text' -> 'ByteString' -> 'Int' -> 'Text'
-- combineTexts name email height =
--   'from' @'Data.Text.Encoding.StrictTextBuilder' $
--     "Height of " <> 'to' name <> " is " <> 'onto' (show height) <> " and email is " <> 'onto' email
-- @
onto :: forall b a. (NormalizesTo a b) => a -> b
onto = onfrom

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @Is a b@ as \"/B/ is /A/\".
--
-- === Laws
--
-- ==== 'from' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'to'
--
-- For all values of /b/ converting from /b/ to /a/ and then converting from /a/ to /b/ produces the original value:
--
-- > \b -> b == from @b (to @a b)
--
-- ==== 'to' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'from'
--
-- For all values of /a/ converting from /a/ to /b/ and then converting from /b/ to /a/ produces the original value:
--
-- > \a -> a == to @a (from @b a)
--
-- ==== Mathematical relationship
--
-- These two laws together establish that 'to' and 'from' form a true [isomorphism](https://en.wikipedia.org/wiki/Isomorphism) between types @a@ and @b@.
-- Note that 'from' is implemented as 'to' from the reverse 'Is' instance, ensuring the symmetry required for isomorphisms.
--
-- ==== 'from' equals 'onfrom'
--
-- For isomorphic types, both ways of converting should be equivalent:
--
-- > \a -> from @b @a a == onfrom @a @b a
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isProperties'.
--
-- === Instance Definition
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require you to define four instances,
-- namely: @Is A B@ and @Is B A@, @NormalizesTo A B@ and @NormalizesTo B A@.
--
-- Instances of @Is@ do not define any functions and serve merely as a statement that the laws are satisfied.
--
-- ==== Example: Lazy Text and Text
--
-- @
-- instance NormalizesTo "Data.Text.Lazy.LazyText" "Data.Text.Text" where
--   to = LazyText.'Data.Text.Lazy.fromStrict'
--   onfrom = LazyText.'Data.Text.Lazy.toStrict'
--
-- instance NormalizesTo "Data.Text.Text" "Data.Text.Lazy.LazyText" where
--   to = LazyText.'Data.Text.Lazy.toStrict'
--   onfrom = LazyText.'Data.Text.Lazy.fromStrict'
--
-- instance Is "Data.Text.Lazy.LazyText" "Data.Text.Text"
--
-- instance Is "Data.Text.Text" "Data.Text.Lazy.LazyText"
-- @
class (NormalizesTo a b, Is b a) => Is a b
