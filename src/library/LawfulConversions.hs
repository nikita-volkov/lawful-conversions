-- |
-- Lawful typeclasses capturing three main patterns of bidirectional mapping. The typeclasses form a layered hierarchy with ascending strictness of laws.
--
-- 1. `IsSome`: Smart constructor
--
-- 2. `IsMany`: Lossy conversion
--
-- 3. `Is`: Isomorphism
--
-- = The conversion problem
--
-- Have you ever looked for a @toString@ function? How often do you
-- import @Data.Text.Lazy@ only to call its 'Data.Text.Lazy.fromStrict'? How
-- about importing @Data.ByteString.Builder@ only to call its
-- 'Data.ByteString.Builder.toLazyByteString' and then importing
-- @Data.ByteString.Lazy@ only to call its 'Data.ByteString.Lazy.toStrict'?
--
-- Those all are instances of one pattern. They are conversions between
-- representations of the same information. Codebases that don't attempt to
-- abstract over this pattern tend to be sprawling with this type of
-- boilerplate. It's noise to the codereader, it's a burden to the
-- implementor and the maintainer.
--
-- = Why another conversion library?
--
-- Many libraries exist that approach the conversion problem. However most of
-- them provide lawless typeclasses leaving it up to the author of the
-- instance to define what makes a proper conversion. This results in
-- inconsistencies across instances, their behaviour not being evident to
-- the user and no way to check whether an instance is correct.
--
-- This library tackles this problem with a lawful typeclass hierarchy, making it
-- evident what any of its instances do and it provides property-tests for you
-- to validate your instances.
--
-- = Conversions
--
-- The main part of the API is two functions: 'to' and 'from'. Both
-- perform a conversion between two types. The main difference between them
-- is in what the first type application parameter specifies. E.g.:
--
-- > toString = to @String
--
-- > fromText = from @Text
--
-- The types should be self-evident:
--
-- > > :t to @String
-- > to @String :: IsSome String b => b -> String
--
-- > > :t from @Text
-- > from @Text :: IsMany Text b => Text -> b
--
-- In other words 'to' and 'from' let you explicitly specify either the source
-- or the target type of a conversion when you need to help the type
-- inferencer or the reader.
--
-- == Examples
--
-- @
-- renderNameAndHeight :: 'Text' -> 'Int' -> 'Text'
-- renderNameAndHeight name height =
--   'from' @'Data.Text.Encoding.StrictTextBuilder' $
--     "Height of " <> 'to' name <> " is " <> 'to' (show height)
-- @
--
-- @
-- combineEncodings :: 'Data.ByteString.Short.ShortByteString' -> 'Data.Primitive.ByteArray' -> ['Word8'] -> 'Data.ByteString.Lazy.ByteString'
-- combineEncodings a b c =
--   'from' @'Data.ByteString.Builder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
--
-- = Partial conversions
--
-- This library also captures the pattern of smart constructors via the 'IsSome' class, which associates a total 'to' conversion with its partial inverse 'maybeFrom'.
--
-- This captures the codec relationship between types.
-- E.g.,
--
-- - Every 'Int16' can be losslessly converted into 'Int32', but not every 'Int32' can be losslessly converted into 'Int16'.
--
-- - Every 'Text' can be converted into 'ByteString' via UTF-8 encoding, but not every 'ByteString' forms a valid UTF-8 sequence.
--
-- - Every URL can be uniquely represented as 'Text', but most 'Text's are not URLs unfortunately.
module LawfulConversions
  ( -- * Typeclasses
    IsSome (..),
    IsMany (..),
    Is,

    -- * Optics
    isSomePrism,
    isManyIso,
    isIso,

    -- * Instance derivation

    -- | Proxy data-types useful for deriving various standard instances using the @DerivingVia@ extension.
    module LawfulConversions.Proxies,

    -- * Testing
    module LawfulConversions.Properties,
  )
where

import LawfulConversions.Classes
import LawfulConversions.Optics
import LawfulConversions.Properties
import LawfulConversions.Proxies
import LawfulConversions.Relations ()
