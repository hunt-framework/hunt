{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UnliftedFFITypes         #-}
module Fox.Types.Term where

import           Fox.IO.Write

import           Data.Bits
import           Data.Primitive
import qualified Data.Text           as Text
import qualified Data.Text.Array     as Arr
import           Data.Text.Internal  (Text (..))
import           Data.Word           (Word8)
import           Foreign.C.Types     (CInt (..), CSize (..))
import           Prelude             hiding (length)
import           System.IO.Unsafe

import           GHC.Base
import           GHC.Exts
import           GHC.Word

data Term = Term {-# UNPACK #-}!ByteArray !Int !Int

instance Eq Term where
  t1 == t2 = equals t1 t2

instance Ord Term where
  compare a b = cmp a b

instance Show Term where
  show (Term _ _ _) = "Term {..}"

fromText :: Text -> Term
fromText (Text (Arr.Array ba) off len) =
  Term (ByteArray ba) off (len `unsafeShiftL` 1)

empty :: Term
empty = fromText Text.empty

null :: Term -> Bool
null (Term _ _ 0) = True
null _            = False

length :: Term -> Int
length (Term _ _ len) = len

commonPrefixes :: Term -> Term -> Maybe (Term, Term)
commonPrefixes (Term (ByteArray ba0) off0 len0)
  (Term (ByteArray ba1) off1 len1) = go 0 0
  where
    go !i !j
      | i < len0 && j < len1 && a == b = go (i + 1) (j + 1)
      | i > 0                          = Just ( Term (ByteArray ba0) off0 i
                                              , Term (ByteArray ba1) (off1 + j) (len1 - j))
      | otherwise                      = Nothing
      where
        !a = case indexWord8Array# ba0 (case off0 + i of I# x -> x) of
               w -> (W8# w)
        !b = case indexWord8Array# ba1 (case off1 + j of I# x -> x) of
               w -> (W8# w)

unsafeWriteTerm :: Term -> Ptr Word8 -> IO (Ptr Word8)
unsafeWriteTerm (Term (ByteArray ba) (I# off) (I# len)) (Ptr op) = IO $ \s ->
  case copyByteArrayToAddr# ba off op len s of
    s' -> (# s', Ptr (plusAddr# op len) #)

termWrite :: Write Term
termWrite = W size write
  where
    W varintSize varintWrite = varint

    size (Term _ _ len) = varintSize len + len

    write term op = do
      varintWrite (length term) op
        >>= unsafeWriteTerm term

equals :: Term -> Term -> Bool
equals (Term (ByteArray ba0) off0 len0) (Term (ByteArray ba1) off1 len1)
  | len0 /= len1 = False
  | otherwise = unsafeDupablePerformIO $ do
      i <- memcmp ba0 (fromIntegral off0) ba1 (fromIntegral off1) (fromIntegral len0)
      return $! i == 0
{-# INLINE equals #-}

cmp :: Term -> Term -> Ordering
cmp (Term (ByteArray ba0) off0 len0) (Term (ByteArray ba1) off1 len1) = do
  unsafeDupablePerformIO $ do
    i <- memcmp ba0 (fromIntegral off0) ba1 (fromIntegral off1) (fromIntegral (min len0 len1))
    return $! case i `compare` 0 of
                EQ -> len0 `compare` len1
                x  -> x
{-# INLINE cmp #-}

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
