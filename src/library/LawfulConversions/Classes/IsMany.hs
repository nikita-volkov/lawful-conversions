module LawfulConversions.Classes.IsMany
  ( module LawfulConversions.Classes.IsMany,
    module LawfulConversions.Classes.IsSome,
  )
where

import LawfulConversions.Classes.IsSome

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
-- ==== 'from' is an [inverse](https://en.wikipedia.org/wiki/Inverse_function) of 'to'
--
-- > \b -> b == from (to @a b)
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
  -- > fromText :: IsMany Text b => Text -> b
  -- > fromText = from @Text
  --
  -- The first type application of the 'to' function on the other hand specifies
  -- the output data type.
  from :: a -> b

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default from :: (IsSome b a) => a -> b
  from = to

instance IsMany a a
