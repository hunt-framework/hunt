{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Count where

import qualified Data.Binary as Binary
import qualified Foreign.Storable as Storable

-- | Represents a count of something; always positive
newtype CountOf a =
  CountOf Int
  deriving (Eq, Ord, Show, Binary.Binary, Storable.Storable)

instance Semigroup (CountOf a) where
  CountOf a <> CountOf b =
    CountOf (a + b)

instance Monoid (CountOf a) where
  mempty =
    zero

isZero :: CountOf a -> Bool
isZero (CountOf 0) = True
isZero _ = False

zero :: CountOf a
zero = CountOf 0

one :: CountOf a
one = CountOf 1

dec :: CountOf a -> CountOf a
dec (CountOf x) =
  if x > 1
  then
    CountOf (x - 1)
  else
    zero

diff :: CountOf a -> CountOf a -> CountOf a
diff (CountOf a) (CountOf b) =
  CountOf (abs (a - b))

getInt :: CountOf a -> Int
getInt (CountOf x) = x
