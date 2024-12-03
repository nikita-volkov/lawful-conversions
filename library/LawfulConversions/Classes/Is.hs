module LawfulConversions.Classes.Is where

import LawfulConversions.Classes.IsMany
import LawfulConversions.Classes.IsSome

-- | Bidirectional conversion between two types with no loss of information.
--
-- The bidirectionality is encoded via a recursive dependency with arguments
-- flipped.
--
-- You can read the signature @Is a b@ as \"/B/ is /A/\".
--
-- === Laws
--
-- /B/ is isomorphic to /A/ if and only if there exists a conversion from /B/
-- to /A/ ('to') and a conversion from /A/ to /B/ ('from') such that:
--
-- - @'from' . 'to' = 'id'@ - For all values of /B/ converting from /B/ to /A/
--     and then converting from /A/ to /B/ produces a value that is identical
--     to the original.
--
-- - @'to' . 'from' = 'id'@ - For all values of /A/ converting from /A/ to /B/
--     and then converting from /B/ to /A/ produces a value that is identical
--     to the original.
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isLawsProperties'.
--
-- === Instance Definition
--
-- For each pair of isomorphic types (/A/ and /B/) the compiler will require
-- you to define four instances, namely: @Is A B@ and @Is B A@ as well as @IsSome A B@ and @IsSome B A@.
--
-- === Testing
--
-- For testing whether your instances conform to these laws use 'LawfulConversions.isLawsProperties'.
class (IsMany a b, Is b a) => Is a b

-- | Any type is isomorphic to itself.
instance Is a a
