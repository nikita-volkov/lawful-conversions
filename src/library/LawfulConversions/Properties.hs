module LawfulConversions.Properties
  ( isSomeProperties,
    isManyProperties,
    isProperties,
  )
where

import LawfulConversions.Algebra
import LawfulConversions.Prelude
import Test.QuickCheck

-- |
-- Properties testing whether an instance satisfies the laws of 'IsSome'.
--
-- The instance is identified via the proxy types that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "IsSome laws" do
-- >     traverse_
-- >       (uncurry prop)
-- >       (isSomeProperties @Int32 @Int16 Proxy Proxy)
isSomeProperties ::
  forall a b.
  (IsSome a b, Eq a, Eq b, Show a, Show b, Arbitrary b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isSomeProperties aProxy bProxy =
  [ ( "'to' is injective",
      property \b1 b2 ->
        b1 /= b2 ==>
          to' b1 =/= to' b2
    ),
    ( "'maybeFrom' is a partial inverse of 'to'",
      property \b ->
        maybeFrom' (to' b) === Just b
    )
  ]
  where
    to' = as aProxy . to . as bProxy
    maybeFrom' = fmap (as bProxy) . maybeFrom . as aProxy

-- |
-- Properties testing whether an instance satisfies the laws of 'IsMany'.
--
-- The instance is identified via the proxy types that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "IsMany laws" do
-- >     traverse_
-- >       (uncurry prop)
-- >       (isManyProperties @String @Text Proxy Proxy)
isManyProperties ::
  forall a b.
  (IsMany a b, Eq a, Eq b, Show a, Show b, Arbitrary a, Arbitrary b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isManyProperties aProxy bProxy =
  [ ( "'onfrom' is an inverse of 'to'",
      property \b -> b === onfrom' (to' b)
    ),
    ( "'onfrom' is consistent with 'maybeFrom'",
      property \(b :: b) ->
        let a = to @a b
         in maybeFrom (to @a b) === Just (onfrom @a @b a)
    ),
    ( "'to' after 'onfrom' always succeeds with 'maybeFrom'",
      property \a ->
        let b = onfrom' a
         in maybeFrom (to' b) === Just b
    )
  ]
    <> isSomeProperties aProxy bProxy
  where
    to' = as aProxy . to . as bProxy
    onfrom' = as bProxy . onfrom . as aProxy

-- |
-- Properties testing whether an instance satisfies the laws of 'Is'.
--
-- The instance is identified via the proxy types that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "Is laws" do
-- >     traverse_
-- >       (uncurry prop)
-- >       (isProperties @Int32 @Word32 Proxy Proxy)
isProperties ::
  (Is a b, Eq a, Eq b, Show a, Show b, Arbitrary a, Arbitrary b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isProperties aProxy bProxy =
  [ ( "'to' is an inverse of 'from'",
      property \b -> b === to' (from' b)
    ),
    ( "'from' equals 'onfrom'",
      property \a -> from' a === onfrom' a
    )
  ]
    <> isManyProperties aProxy bProxy
  where
    to' = as aProxy . to . as bProxy
    from' = as bProxy . from . as aProxy
    onfrom' = as bProxy . onfrom . as aProxy
