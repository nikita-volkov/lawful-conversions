module LawfulConversions.Proxies.ViaIsSome where

import LawfulConversions.Classes
import LawfulConversions.Prelude
import qualified Test.QuickCheck as QuickCheck

-- |
-- Helper for deriving common instances on types which have an instance of @'IsSome' sup@ using the @DerivingVia@ extension.
--
-- E.g.,
--
-- > newtype Percent = Percent Double
-- >   deriving newtype (Show, Eq, Ord)
-- >   deriving (Read, Arbitrary) via (ViaIsSome Double Percent)
-- >
-- > instance IsSome Double Percent where
-- >   to (Percent double) = double
-- >   maybeFrom double =
-- >     if double < 0 || double > 1
-- >       then Nothing
-- >       else Just (Percent double)
--
-- In the code above all the instances that are able to construct the values of 'Percent' are automatically derived based on the @IsSome Double Percent@ instance.
-- This guarantees that they only construct values that pass thru the checks defined in 'maybeFrom'.
newtype ViaIsSome sup sub = ViaIsSome sub

instance (IsSome sup sub) => IsSome sup (ViaIsSome sup sub) where
  to (ViaIsSome a) = to a
  maybeFrom = fmap ViaIsSome . maybeFrom

instance IsSome sub (ViaIsSome sup sub) where
  to = coerce

instance IsSome (ViaIsSome sup sub) sub where
  to = coerce

instance IsMany sub (ViaIsSome sup sub)

instance IsMany (ViaIsSome sup sub) sub

instance Is sub (ViaIsSome sup sub)

instance Is (ViaIsSome sup sub) sub

instance (IsSome sup sub, Show sup) => Show (ViaIsSome sup sub) where
  show (ViaIsSome a) = show (to @sup a)

instance (IsSome sup sub, Read sup) => Read (ViaIsSome sup sub) where
  readPrec = do
    sup <- readPrec
    case maybeFrom @sup sup of
      Just a -> pure (ViaIsSome a)
      Nothing -> fail "Value is not from the subset"

instance (IsSome sup sub, IsString sup) => IsString (ViaIsSome sup sub) where
  fromString =
    maybe (error "Value is not from the subset") ViaIsSome . maybeFrom @sup . fromString

instance (IsSome sup sub, Eq sup) => Eq (ViaIsSome sup sub) where
  (==) = on (==) (to @sup)

instance (IsSome sup sub, Ord sup) => Ord (ViaIsSome sup sub) where
  compare = on compare (to @sup)

instance (IsSome sup sub, QuickCheck.Arbitrary sup) => QuickCheck.Arbitrary (ViaIsSome sup sub) where
  arbitrary =
    QuickCheck.suchThatMap QuickCheck.arbitrary (maybeFrom @sup)
  shrink value = do
    shrunkValue <- QuickCheck.shrink (to @sup value)
    shrunkValue
      & maybeFrom
      & maybeToList
