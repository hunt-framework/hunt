{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE Unsafe          #-}
module Data.Primitive.PrimRef (
    module Data.Primitive.PrimRef
  , PrimState
  ) where

import           Control.Monad.Primitive
import           Data.Primitive
import           GHC.Prim
import           GHC.Types               (Int (I#))

newtype PrimRef s a = PrimRef (MutableByteArray s)

instance Eq (PrimRef s a) where
  PrimRef m == PrimRef n = sameMutableByteArray m n
  {-# INLINE (==) #-}

-- | Create a primitive reference.
newPrimRef :: (PrimMonad m, Prim a) => a -> m (PrimRef (PrimState m) a)
newPrimRef a = do
  m <- newByteArray (sizeOf a)
  writeByteArray m 0 a
  return (PrimRef m)
{-# INLINE newPrimRef #-}

-- | Read a primitive value from the reference
readPrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> m a
readPrimRef (PrimRef m) = readByteArray m 0
{-# INLINE readPrimRef #-}

-- | Write a primitive value to the reference
writePrimRef :: (PrimMonad m, Prim a) => PrimRef (PrimState m) a -> a -> m ()
writePrimRef (PrimRef m) a = writeByteArray m 0 a
{-# INLINE writePrimRef #-}

-- | Given a reference, and a value to add, atomically add the value to the
-- element. Returns the value of the element before the operation.
-- Implies a full memory barrier.
fetchAddInt :: PrimMonad m => PrimRef (PrimState m) Int -> Int -> m Int
fetchAddInt (PrimRef (MutableByteArray m)) (I# x) =
  primitive $ \s -> case fetchAddIntArray# m 0# x s of
    (# s', result #) -> (# s', I# result #)
{-# INLINE fetchAddInt #-}
