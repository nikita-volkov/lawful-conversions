module LawfulConversions.Laws
  ( isSomeLawsProperties,
    isLawsProperties,
  )
where

import LawfulConversions.Classes
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
-- >       (isSomeLawsProperties @Int32 @Int16 Proxy Proxy)
isSomeLawsProperties ::
  (IsSome a b, Eq a, Eq b, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isSomeLawsProperties superp subp =
  [ ( "'to' is injective",
      property \a b ->
        a /= b ==>
          to' a =/= to' b
    ),
    ( "'maybeFrom' is an inverse of 'to'",
      property \a ->
        maybeFrom' (to' a) === Just a
    )
  ]
  where
    to' = as superp . to . as subp
    maybeFrom' = fmap (as subp) . maybeFrom . as superp
    as = flip asProxyTypeOf

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
-- >       (isLawsProperties @Int32 @Word32 Proxy Proxy)
isLawsProperties ::
  (Is a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isLawsProperties superp subp =
  [ directedLaws "↻" superp subp,
    directedLaws "↺" subp superp
  ]
    & mconcat
  where
    directedLaws prefix ap bp =
      ( ( "Isomorphic: Law 1",
          property \b ->
            b === to (asProxyTypeOf (to (asProxyTypeOf b bp)) ap)
        )
          : prefixEachName "Partially isomorphic: " (isSomeLawsProperties ap bp)
      )
        & prefixEachName (prefix <> ": ")

prefixEachName ::
  String ->
  [(String, Property)] ->
  [(String, Property)]
prefixEachName prefix =
  (fmap . first) (mappend prefix)
