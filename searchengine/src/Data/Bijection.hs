--{-# LANGUAGE FlexibleInstances    #-}
--{-# LANGUAGE UndecidableInstances #-}

module Data.Bijection where

-- | Bijective function 'to' and it's reverse 'from'.
class Bijection a b where
  to   :: a -> b
  from :: b -> a

-- This works, but requires XFlexibleInstances XUndecidableInstances which causes errors not being
-- checked by the compiler. Probably because the constraint is not smaller than the head (Paterson-
-- Condition?), which is allowed with XUndecidableInstances.
{-
-- | Only one way needs to be defined.
instance Bijection a b => Bijection b a where
  to   = from
  from = to
-}
