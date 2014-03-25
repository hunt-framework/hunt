{-# LANGUAGE MultiParamTypeClasses #-}

--{-# LANGUAGE FlexibleInstances    #-}
--{-# LANGUAGE UndecidableInstances #-}

{- |
'Bijection' instances represent a bijection between two types and allow conversion to and from.
-}

module Data.Bijection where

-- | Bijection between two types @a@ and @b@.
--   'to' and 'from' represent the bijective function.
--
-- For a proper bijection between x and y, two instances need to be defined (@Bijection x y@ and
-- @Bijection y x@).
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
