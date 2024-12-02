{-# LANGUAGE CPP #-}

-- |
-- Lawful solution to the conversion problem.
--
-- = Conversion problem
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
-- This library tackles this problem with a lawful typeclass, making it
-- evident what any of its instances do and it provides property-tests for you
-- to validate your instances.
--
-- = The insight
--
-- The key insight of this library is that if you add a requirement for the
-- conversion to be lossless and to have a mirror conversion in the opposite
-- direction, there usually appears to be only one way of defining it. That
-- makes it very clear what the conversion does to the user and how to define
-- it to the author of the conversion.
-- It also gives a clear criteria for validating whether the instances are correct, which can be encoded in property-tests.
--
-- That insight itself stems from an observation that almost all of the
-- practical conversions in Haskell share a property: you can restore the
-- original data from its converted form. E.g., you can get a text from
-- a text-builder and you can create a text-builder from a text, you can convert
-- a bytestring into a list of bytes and vice-versa, bytestring to\/from bytearray,
-- strict bytestring to\/from lazy, list to\/from sequence, sequence to/from
-- vector, set of ints to\/from int-set. In other words, it's always a two-way
-- street with them and there's a lot of instances of this pattern.
--
-- = UX
--
-- A few other accidental findings like encoding this property with recursive
-- typeclass constraints and fine-tuning for the use of
-- the @TypeApplications@ extension resulted in a terse and clear API.
--
-- Essentially the whole API is just two functions: 'to' and 'from'. Both
-- perform a conversion between two types. The only difference between them
-- is in what the first type application parameter specifies. E.g.:
--
-- > toString = to @String
--
-- > fromText = from @Text
--
-- The types are self-evident:
--
-- > > :t to @String
-- > to @String :: Is String b => b -> String
--
-- > > :t from @Text
-- > from @Text :: Is Text b => Text -> b
--
-- In other words 'to' and 'from' let you explicitly specify either the source
-- or the target type of a conversion when you need to help the type
-- inferencer.
--
-- Here are more practical examples:
--
-- @
-- renderNameAndHeight :: 'Text' -> 'Int' -> 'Text'
-- renderNameAndHeight name height =
--   'from' @'Data.Text.Encoding.StrictTextBuilder' $
--     "Height of " <> 'to' name <> " is " <> 'to' (show height)
-- @
--
-- @
-- combineEncodings :: 'Data.ByteString.Short.ShortByteString' -> 'Data.Primitive.ByteArray' -> 'Data.ByteString.Lazy.ByteString' -> [Word8]
-- combineEncodings a b c =
--   'from' @'Data.ByteString.Builder.Builder' $
--     'to' a <> 'to' b <> 'to' c
-- @
--
-- = Partial conversions
--
-- Atop of all said this library also captures the notion of smart constructors via the 'IsSome' class, which associates a total 'to' conversion with partial 'maybeFrom'.
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
    Is,
    IsSome (..),
    from,

    -- * Optics
    isSomePrism,
    isIso,

    -- * Instance derivation

    -- | Proxy data-types useful for deriving various standard instances using the @DerivingVia@ extension.
    module LawfulConversions.Proxies,

    -- * Testing
    module LawfulConversions.Laws,
  )
where

import LawfulConversions.Classes
import LawfulConversions.Laws
import LawfulConversions.Optics
import LawfulConversions.Proxies
import LawfulConversions.Relations ()
