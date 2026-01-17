module LawfulConversions.Proxies.ViaIsSubsetOf where

import LawfulConversions.Algebra
import LawfulConversions.Prelude
import qualified Test.QuickCheck as QuickCheck

-- |
-- Helper for deriving common instances on types which have an instance of @'IsSubsetOf' a@ using the @DerivingVia@ extension.
--
-- E.g.,
--
-- > newtype Percent = Percent Double
-- >   deriving newtype (Show, Eq, Ord)
-- >   deriving (Read, Arbitrary) via (ViaIsSubsetOf Double Percent)
-- >
-- > instance IsSubsetOf Double Percent where
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
-- In the code above all the instances that are able to construct the values of 'Percent' are automatically derived based on the @IsSubsetOf Double Percent@ instance.
-- This guarantees that they only construct values that pass thru the checks defined in 'maybeFrom'.
newtype ViaIsSubsetOf a b = ViaIsSubsetOf b

instance (IsSubsetOf a b) => IsSubsetOf a (ViaIsSubsetOf a b) where
  to (ViaIsSubsetOf a) = to a
  maybeFrom = fmap ViaIsSubsetOf . maybeFrom
  onfrom = ViaIsSubsetOf . onfrom

instance IsSubsetOf b (ViaIsSubsetOf a b) where
  to = coerce
  onfrom = coerce

instance IsSubsetOf (ViaIsSubsetOf a b) b where
  to = coerce
  onfrom = coerce

instance Is b (ViaIsSubsetOf a b)

instance Is (ViaIsSubsetOf a b) b

instance (IsSubsetOf a b, Show a) => Show (ViaIsSubsetOf a b) where
  show (ViaIsSubsetOf a) = show (to @a a)

instance (IsSubsetOf a b, Read a) => Read (ViaIsSubsetOf a b) where
  readPrec = do
    a <- readPrec
    case maybeFrom @a a of
      Just a -> pure (ViaIsSubsetOf a)
      Nothing -> fail "Value is not from the subset"

instance (IsSubsetOf a b, IsString a) => IsString (ViaIsSubsetOf a b) where
  fromString =
    maybe (error "Value is not from the subset") ViaIsSubsetOf . maybeFrom @a . fromString

instance (IsSubsetOf a b, Eq a) => Eq (ViaIsSubsetOf a b) where
  (==) = on (==) (to @a)

instance (IsSubsetOf a b, Ord a) => Ord (ViaIsSubsetOf a b) where
  compare = on compare (to @a)

instance (IsSubsetOf a b, QuickCheck.Arbitrary a) => QuickCheck.Arbitrary (ViaIsSubsetOf a b) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @a)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @a value)
    shrunkValue
      & maybeFrom
      & maybeToList
