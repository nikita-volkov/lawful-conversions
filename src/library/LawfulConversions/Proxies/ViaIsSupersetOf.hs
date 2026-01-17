module LawfulConversions.Proxies.ViaIsSupersetOf where

import LawfulConversions.Algebra
import LawfulConversions.Prelude
import qualified Test.QuickCheck as QuickCheck

-- |
-- Helper for deriving common instances on types which have an instance of @'IsSupersetOf' a@ using the @DerivingVia@ extension.
--
-- E.g.,
--
-- > newtype Percent = Percent Double
-- >   deriving newtype (Show, Eq, Ord)
-- >   deriving (Read, Arbitrary) via (ViaIsSupersetOf Double Percent)
-- >
-- > instance IsSupersetOf Double Percent where
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
-- In the code above all the instances that are able to construct the values of 'Percent' are automatically derived based on the @IsSupersetOf Double Percent@ instance.
-- This guarantees that they only construct values that pass thru the checks defined in 'maybeFrom'.
newtype ViaIsSupersetOf a b = ViaIsSupersetOf b

instance (IsSupersetOf a b) => IsSupersetOf a (ViaIsSupersetOf a b) where
  to (ViaIsSupersetOf a) = to a
  maybeFrom = fmap ViaIsSupersetOf . maybeFrom
  onfrom = ViaIsSupersetOf . onfrom

instance IsSupersetOf b (ViaIsSupersetOf a b) where
  to = coerce
  onfrom = coerce

instance IsSupersetOf (ViaIsSupersetOf a b) b where
  to = coerce
  onfrom = coerce

instance Is b (ViaIsSupersetOf a b)

instance Is (ViaIsSupersetOf a b) b

instance (IsSupersetOf a b, Show a) => Show (ViaIsSupersetOf a b) where
  show (ViaIsSupersetOf a) = show (to @a a)

instance (IsSupersetOf a b, Read a) => Read (ViaIsSupersetOf a b) where
  readPrec = do
    a <- readPrec
    case maybeFrom @a a of
      Just a -> pure (ViaIsSupersetOf a)
      Nothing -> fail "Value is not from the subset"

instance (IsSupersetOf a b, IsString a) => IsString (ViaIsSupersetOf a b) where
  fromString =
    maybe (error "Value is not from the subset") ViaIsSupersetOf . maybeFrom @a . fromString

instance (IsSupersetOf a b, Eq a) => Eq (ViaIsSupersetOf a b) where
  (==) = on (==) (to @a)

instance (IsSupersetOf a b, Ord a) => Ord (ViaIsSupersetOf a b) where
  compare = on compare (to @a)

instance (IsSupersetOf a b, QuickCheck.Arbitrary a) => QuickCheck.Arbitrary (ViaIsSupersetOf a b) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @a)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @a value)
    shrunkValue
      & maybeFrom
      & maybeToList
