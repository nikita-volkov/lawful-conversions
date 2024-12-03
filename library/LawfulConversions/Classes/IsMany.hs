module LawfulConversions.Classes.IsMany where

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
-- ==== 'from' is an inverse of 'to'
--
-- > \sup -> sup == from (to sup)
class (IsSome sup sub) => IsMany sup sub where
  -- |
  -- Possibly lossy inverse of 'to'.
  -- [Surjection](https://en.wikipedia.org/wiki/Surjective_function) from @sup@ to @sub@.
  --
  -- You can think of it as a version of 'maybeFrom', which provides default handling of cases where 'maybeFrom' would have produced 'Nothing'.
  --
  -- Particularly useful in combination with the @TypeApplications@ extension,
  -- where it allows to specify the input type, e.g.:
  --
  -- > fromText :: IsMany Text sub => Text -> sub
  -- > fromText = from @Text
  --
  -- The first type application of the 'to' function on the other hand specifies
  -- the output data type.
  from :: sup -> sub

  -- |
  -- Requires the presence of 'IsSome' in reverse direction.
  default from :: (IsSome sub sup) => sup -> sub
  from = to

instance IsMany sup sup
