module LawfulConversions.Classes.Is
  ( module LawfulConversions.Classes.Is,
    module LawfulConversions.Classes.IsMany,
  )
where

import LawfulConversions.Classes.IsMany

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
-- > \b -> b == from (to @a b)
--
-- ==== 'to' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'from'
--
-- For all values of /a/ converting from /a/ to /b/ and then converting from /b/ to /a/ produces the original value:
--
-- > \a -> a == to (from @a @b a)
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
-- ==== __Example: Lazy Text and Text__
--
-- @
-- instance IsSome 'Data.Text.Lazy.LazyText' 'Data.Text.Text' where
--   to = LazyText.'Data.Text.Lazy.fromStrict'
--
-- instance IsSome 'Data.Text.Text' 'Data.Text.Lazy.LazyText' where
--   to = LazyText.'Data.Text.Lazy.toStrict'
--
-- instance IsMany 'Data.Text.Lazy.LazyText' 'Data.Text.Text'
--
-- instance IsMany 'Data.Text.Text' 'Data.Text.Lazy.LazyText'
--
-- instance Is 'Data.Text.Lazy.LazyText' 'Data.Text.Text'
--
-- instance Is 'Data.Text.Text' 'Data.Text.Lazy.LazyText'
-- @
class (IsMany a b, Is b a) => Is a b

-- | Any type is isomorphic to itself.
instance Is a a
