{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}
module Fox.IO.Write (
    Write(..)
  , (>*<)
  , (>$<)
  , word8
  , word64
  , varint
  , bytestring
  , text
  ) where

import           GHC.Exts
import           GHC.Types

import           Data.Bits
import           Data.ByteString.Internal             (ByteString(..))
import qualified Data.ByteString.Internal             as ByteString
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Int
import           Data.Text                            (Text)
import qualified Data.Text.Foreign                    as Text
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable

-- | Low-level primitive for writing to a buffer.
-- @Write@s can be composed using a contravariant interface.
data Write a = W (a -> Int) (a -> Ptr Word8 -> IO (Ptr Word8))

instance Contravariant Write where
  contramap f (W hint write) = W (hint . f) (write . f)
  {-# INLINE CONLIKE contramap #-}

instance Divisible Write where
  divide = divideW
  {-# INLINE CONLIKE divide #-}

  conquer = unitW
  {-# INLINE CONLIKE conquer #-}

unitW :: Write a
unitW = W (\_ -> 0) (\_ op -> return op)
{-# INLINE CONLIKE unitW #-}

divideW :: (a -> (b, c)) -> Write b -> Write c -> Write a
divideW f (W hint1 write1) (W hint2 write2) =
  W (\a -> let (x, y) = f a in hint1 x + hint2 y)
    (\a op -> let (x, y) = f a in write1 x op >>= write2 y)
{-# INLINE CONLIKE divideW #-}

(>*<) :: Write a -> Write b -> Write (a, b)
(>*<) = divided
{-# INLINE CONLIKE (>*<) #-}
infixr 5 >*<

fromStorable :: Storable a => Write a
fromStorable = W
               (\a -> sizeOf a)
               (\a op -> poke (castPtr op) a >> pure (op `plusPtr` sizeOf a))
{-# INLINE CONLIKE fromStorable #-}

word8 :: Write Word8
word8 = fromStorable
{-# INLINE CONLIKE word8 #-}

be64 :: Word64 -> Word64
be64 = byteSwap64
{-# INLINE be64 #-}

word64 :: Write Word64
word64 = be64 >$< fromStorable
{-# INLINE CONLIKE word64 #-}

-- NB. be careful here with INLINE pragmas!
-- Annotating `varword` with INLINE duplicates the small (and very fast)
-- inner loop all over the place.
varword :: Write Word
varword = W (\_ -> 9) go
  where
    go (W# w) (Ptr p) = IO $ \s0 -> case loop w p s0 of
                                         (# s1, p' #) -> (# s1, Ptr p' #)

    loop w p s0
      | isTrue# (ltWord# w (int2Word# 0x80#)) =
          case writeWord8OffAddr# p 0# (narrow8Word# w) s0 of
            s1 -> (# s1, plusAddr# p 1# #)
      | otherwise =
        case writeWord8OffAddr# p 0# ((narrow8Word# (or# w (int2Word# 0x80#)))) s0 of
          s1 -> loop (uncheckedShiftRL# w 7#) (plusAddr# p 1#) s1

text :: Write Text
text = W size write
  where
    W vintSize vintWrite = varint

    size t = case Text.lengthWord16 t `unsafeShiftL` 1 of
               tsz -> vintSize tsz + tsz

    write t op =  case Text.lengthWord16 t `unsafeShiftL` 1 of
                    tsz -> do op'  <- vintWrite tsz op
                              Text.unsafeCopyToPtr t $! (castPtr op')
                              return $! op' `plusPtr` tsz
{-# INLINE CONLIKE text #-}

bytestring :: Write ByteString
bytestring = W size write
  where
    W vintSize vintWrite = varint

    size (PS _ _ len) = vintSize len + len

    write (PS fpb off len) op = do
      op' <- vintWrite len op
      withForeignPtr fpb $ \src ->
        ByteString.memcpy op' (src `plusPtr` off) len
      return (op' `plusPtr` len)

varint64 :: Write Int64
varint64 = fromIntegral >$< varword
{-# INLINE CONLIKE varint64 #-}

varint :: Write Int
varint = fromIntegral >$< varint64
{-# INLINE CONLIKE varint #-}
