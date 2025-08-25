module LawfulConversions.Algebra where

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
-- ==== 'maybeFrom' characterizes the image of 'to'
--
-- 'maybeFrom' succeeds exactly on values that are in the image of 'to':
--
-- > \a b -> maybeFrom a == Just b ==> to b == a
--
-- ==== Mathematical foundation
--
-- These laws establish that type @b@ forms a subset of type @a@ in the mathematical sense.
-- The 'to' function provides the canonical injection, while 'maybeFrom' recognizes which values of @a@
-- correspond to values from the subset @b@.
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

-- |
-- Convert a value of a superset type to a subset type specifying the superset type first.
--
-- Alias to 'to' with the only difference in the argument order.
--
-- E.g.,
--
-- > fromText = from @Text
from :: forall b a. (IsSome a b) => b -> a
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
-- E.g.,
--
-- > result = maybeTo @Percent someDouble
maybeTo :: forall b a. (IsSome a b) => a -> Maybe b
maybeTo = maybeFrom

-- |
-- Lossy or canonicalizing conversion.
-- Captures mappings from multiple alternative inputs into one output.
--
-- E.g.,
--
-- - `ByteString` can be decoded into `Text` with UTF-8 leniently, replacing the invalid chars with a default char.
--
-- - `String` has a wider range of supported chars than `Text`, so some chars get replaced too.
--
-- === Laws
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
-- ==== Law hierarchy
--
-- 'IsMany' extends 'IsSome', so all laws from 'IsSome' also apply here.
-- The combination ensures that 'onfrom' provides a canonical (possibly lossy) conversion from @a@ to @b@,
-- while 'to' provides the lossless injection from @b@ to @a@.
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isManyProperties'.
class (IsSome a b) => IsMany a b where
  -- |
  -- Possibly lossy inverse of 'to'.
  -- [Surjection](https://en.wikipedia.org/wiki/Surjective_function) from @a@ to @b@.
  --
  -- Particularly useful in combination with the @TypeApplications@ extension,
  -- where it allows to specify the input type, e.g.:
  --
  -- > fromString :: IsMany String b => String -> b
  -- > fromString = onfrom @String
  --
  -- If you want to specify the output type instead, use 'onto'.
  onfrom :: a -> b

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default onfrom :: (IsSome b a) => a -> b
  onfrom = to

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
onto :: forall b a. (IsMany a b) => a -> b
onto = onfrom

instance IsMany a a

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
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require you to define six instances, namely: @Is A B@ and @Is B A@, @IsMany A B@ and @IsMany B A@, @IsSome A B@ and @IsSome B A@.
--
-- Instances of @Is@ do not define any functions and serve merely as a statement that the laws are satisfied.
--
-- ==== Example: Lazy Text and Text
--
-- @
-- instance IsSome "Data.Text.Lazy.LazyText" "Data.Text.Text" where
--   to = LazyText.'Data.Text.Lazy.fromStrict'
--
-- instance IsSome "Data.Text.Text" "Data.Text.Lazy.LazyText" where
--   to = LazyText.'Data.Text.Lazy.toStrict'
--
-- instance IsMany "Data.Text.Lazy.LazyText" "Data.Text.Text"
--
-- instance IsMany "Data.Text.Text" "Data.Text.Lazy.LazyText"
--
-- instance Is "Data.Text.Lazy.LazyText" "Data.Text.Text"
--
-- instance Is "Data.Text.Text" "Data.Text.Lazy.LazyText"
-- @
class (IsMany a b, Is b a) => Is a b

-- | Any type is isomorphic to itself.
instance Is a a
