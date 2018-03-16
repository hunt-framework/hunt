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

zero :: CountOf a
zero = CountOf 0

one :: CountOf a
one = CountOf 1

getInt :: CountOf a -> Int
getInt (CountOf x) = x
