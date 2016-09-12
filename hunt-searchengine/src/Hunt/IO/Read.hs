{-# LANGUAGE RankNTypes #-}
module Hunt.IO.Read where

import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable

data Decoder a = D !Int (forall r. (a -> Ptr Word8 -> IO r) -> Ptr Word8 -> IO r)

instance Functor Decoder where
  fmap f (D n m) = D n $ \k op -> m (k . f) op
  {-# INLINE fmap #-}

pairD :: Decoder a -> Decoder b -> Decoder (a, b)
pairD (D n1 f) (D n2 g) = D (n1 + n2) (\k -> f (\a -> g (\b -> k (a, b))))
{-# INLINE pairD #-}

(>*<) :: Decoder a -> Decoder b -> Decoder (a, b)
(>*<) = pairD
{-# INLINE (>*<) #-}
infixr 5 >*<

runDecoder :: Decoder a -> Ptr Word8 -> IO (a, Ptr Word8)
runDecoder (D _ f) = f (\a op' -> return (a, op'))
{-# INLINE runDecoder #-}

varint :: Decoder Word64
varint = D 9 step
  where
    step k op0 = do
          x <- peek op0
          go (op0 `plusPtr` 1) x 0
          where  go op x acc
                   | testBit x 7 = undefined
                   | otherwise   = k acc op
{-# INLINE varint #-}

byteString :: Int -> Decoder ByteString
byteString n = D n $ \k op ->
  k (ByteString.unsafeCreate n $ \buf ->
        ByteString.memcpy buf op n) (op `plusPtr` n)
{-# INLINE byteString #-}
