{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Control.X.AlaCarte where

absurd :: a
absurd = error "This makes no sense"

data f :+: g = InL f | InR g

infixr 6 :+:

class f :<: g where
  inject :: f -> g
  outject :: g -> Maybe f

instance f :<: f where
  inject = id
  {-# INLINE inject #-}

  outject = Just
  {-# INLINE outject #-}

instance {-# OVERLAPS #-} f :<: (f :+: g) where
  inject = InL
  {-# INLINE inject #-}

  outject (InL x) = Just x
  outject (InR _) = absurd
  {-# INLINE outject #-}

instance {-# OVERLAPPABLE #-} (f :<: g) => f :<: (h :+: g) where
  inject = InR . inject
  outject (InR x) = outject x
  outject (InL _) = Nothing

instance (Show f, Show g) => Show (f :+: g) where
  show (InR x) = show x
  show (InL x) = show x

-- | Match a type @f@ that is injectable into @g@.
-- Note that a @_@ match(not @U _@) causes all matches to fail, so
-- ignore the incomplete matches warning
pattern U :: (f :<: g) => f -> g
pattern U x <-
  (outject -> Just x)
  where
    U x = inject x

-- Let's make everyone happy
type MemberOf f g = f :<: g

type Throws f g = f :+: g

throwOne :: (MemberOf f g) => f -> g
throwOne = inject

catchOne :: (MemberOf f g) => g -> Maybe f
catchOne = outject
