{-# LANGUAGE RecordWildCards #-}
module Hunt.IO.Buffer where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Internal  as ByteString
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Ptr

data Buffer =
  Buffer { bufSize  :: !Int
         , bufStart :: !(Ptr Word8)
         , bufEnd   :: !(Ptr Word8)
         , bufPtr   :: !(Ptr Word8)
         , buffPtr  :: !(ForeignPtr Word8)
         }

newBuffer :: Int -> IO Buffer
newBuffer sz = do
  buf <- mallocForeignPtrBytes sz
  let ptr = unsafeForeignPtrToPtr buf
  return Buffer {
      bufSize = sz
    , bufStart = ptr
    , bufEnd = ptr `plusPtr` sz
    , bufPtr = ptr
    , buffPtr = buf
    }

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer bufSz f = do
  buf <- newBuffer bufSz
  f buf
{-# INLINE withBuffer #-}

reset :: Buffer -> Buffer
reset buf = buf { bufStart = bufPtr buf }
{-# INLINE reset #-}

null :: Buffer -> Bool
null Buffer{..} = bufStart == bufPtr
{-# INLINE null #-}

hasEnoughBytes :: Buffer -> Int -> Bool
hasEnoughBytes Buffer{..} n =
  bufEnd `minusPtr` bufStart >= n
{-# INLINE hasEnoughBytes #-}

toByteString :: Buffer -> ByteString
toByteString Buffer{..} =
  ByteString.fromForeignPtr buffPtr 0 (bufStart `minusPtr` bufPtr)
{-# INLINE toByteString #-}

put :: Buffer
    -> (Ptr Word8 -> IO (Ptr Word8))
    -> IO Buffer
put buf insert = do
  start' <- insert (bufStart buf)
  return buf { bufStart = start' }
{-# INLINE put #-}

putByteString :: Buffer
              -> ByteString
              -> IO Buffer
putByteString buf s = do
  let (fop, off, len) = ByteString.toForeignPtr s
  withForeignPtr fop $ \op -> do
    ByteString.memcpy (bufStart buf) (op `plusPtr` off) len
    return buf { bufStart = bufStart buf `plusPtr` len
               }
{-# INLINE putByteString #-}
