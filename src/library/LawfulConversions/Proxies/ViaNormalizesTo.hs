module LawfulConversions.Proxies.ViaNormalizesTo where

import LawfulConversions.Algebra
import LawfulConversions.Prelude
import qualified Test.QuickCheck as QuickCheck

-- |
-- Helper for deriving common instances on types which have an instance of @'NormalizesTo' a@ using the @DerivingVia@ extension.
--
-- E.g.,
--
-- > newtype Percent = Percent Double
-- >   deriving newtype (Show, Eq, Ord)
-- >   deriving (Read, Arbitrary) via (ViaNormalizesTo Double Percent)
-- >
-- > instance NormalizesTo Double Percent where
-- >   to (Percent double) = double
-- >   maybeFrom double =
-- >     if double < 0 || double > 1
-- >       then Nothing
-- >       else Just (Percent double)
-- >   onfrom double =
-- >     if double < 0
-- >       then Percent 0
-- >       else if double > 1
-- >         then Percent 1
-- >         else Percent double
--
-- In the code above all the instances that are able to construct the values of 'Percent' are automatically derived based on the @NormalizesTo Double Percent@ instance.
-- This guarantees that they only construct values that pass thru the checks defined in 'maybeFrom'.
newtype ViaNormalizesTo a b = ViaNormalizesTo b

instance (NormalizesTo a b) => NormalizesTo a (ViaNormalizesTo a b) where
  to (ViaNormalizesTo a) = to a
  maybeFrom = fmap ViaNormalizesTo . maybeFrom
  onfrom = ViaNormalizesTo . onfrom

instance NormalizesTo b (ViaNormalizesTo a b) where
  to = coerce
  onfrom = coerce

instance NormalizesTo (ViaNormalizesTo a b) b where
  to = coerce
  onfrom = coerce

instance Is b (ViaNormalizesTo a b)

instance Is (ViaNormalizesTo a b) b

instance (NormalizesTo a b, Show a) => Show (ViaNormalizesTo a b) where
  show (ViaNormalizesTo a) = show (to @a a)

instance (NormalizesTo a b, Read a) => Read (ViaNormalizesTo a b) where
  readPrec = do
    a <- readPrec
    case maybeFrom @a a of
      Just a -> pure (ViaNormalizesTo a)
      Nothing -> fail "Value is not from the subset"

instance (NormalizesTo a b, IsString a) => IsString (ViaNormalizesTo a b) where
  fromString =
    maybe (error "Value is not from the subset") ViaNormalizesTo . maybeFrom @a . fromString

instance (NormalizesTo a b, Eq a) => Eq (ViaNormalizesTo a b) where
  (==) = on (==) (to @a)

instance (NormalizesTo a b, Ord a) => Ord (ViaNormalizesTo a b) where
  compare = on compare (to @a)

instance (NormalizesTo a b, QuickCheck.Arbitrary a) => QuickCheck.Arbitrary (ViaNormalizesTo a b) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @a)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @a value)
    shrunkValue
      & maybeFrom
      & maybeToList
