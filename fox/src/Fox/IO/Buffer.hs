module Fox.IO.Buffer (
    WriteBuffer
  , withWriteBuffer
  , offset
  , write
  , writeByteString
  ) where

import           Control.Exception
import qualified Data.ByteString.Internal as ByteString
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

-- | Low-level off-heap buffer primitive.
newtype Buffer = Buffer (Ptr Word8)

sizeOfInt :: Int
sizeOfInt = sizeOf (undefined :: Int)

sizeOfPtr :: Int
sizeOfPtr = sizeOf (undefined :: Ptr ())

bufferStart :: Buffer -> Ptr Word8
bufferStart (Buffer op) =
  (castPtr op :: Ptr Word8) `plusPtr` (2 * sizeOfPtr + 1 * sizeOfInt)
{-# INLINE bufferStart #-}

peekPos :: Buffer -> IO (Ptr Word8)
peekPos (Buffer op) = peek (castPtr op)
{-# INLINE peekPos #-}

pokePos :: Buffer -> Ptr Word8 -> IO ()
pokePos (Buffer op) pos = poke (castPtr op) pos
{-# INLINE pokePos #-}

peekEnd :: Buffer -> IO (Ptr Word8)
peekEnd (Buffer op) = peekElemOff (castPtr op) 1
{-# INLINE peekEnd #-}

pokeEnd :: Buffer -> Ptr Word8 -> IO ()
pokeEnd (Buffer op) end = pokeElemOff (castPtr op) 1 end
{-# INLINE pokeEnd #-}

peekBytesWritten :: Buffer -> IO Int
peekBytesWritten (Buffer op) =
  peek (castPtr (op `plusPtr` (2 * sizeOfPtr)))
{-# INLINE peekBytesWritten #-}

pokeBytesWritten :: Buffer -> Int -> IO ()
pokeBytesWritten (Buffer op) n =
  poke (castPtr (op `plusPtr` (2 * sizeOfPtr))) n
{-# INLINE pokeBytesWritten #-}

newBuffer :: Int -> IO Buffer
newBuffer bufSize = do
  -- we use cs malloc here to avoid
  -- copying this buffer during GCs.
  buf <- mallocBytes (bufSize + 2 * sizeOfPtr + 1 * sizeOfInt)
  pokePos (Buffer buf) buf
  pokeEnd (Buffer buf) (buf `plusPtr` bufSize)
  pokeBytesWritten (Buffer buf) 0
  return (Buffer buf)

freeBuffer :: Buffer -> IO ()
freeBuffer (Buffer buf) = free buf

size :: Buffer -> IO Int
size buf = do
  let start = bufferStart buf
  end <- peekEnd buf
  return $ end `minusPtr` start
{-# INLINE size #-}

hasEnoughBytes :: Buffer -> Int -> IO Bool
hasEnoughBytes buf n = do
  pos <- peekPos buf
  end <- peekEnd buf
  return $ end `minusPtr` pos >= n
{-# INLINE hasEnoughBytes #-}

put :: Buffer -> (Ptr Word8 -> IO (Ptr Word8)) -> IO Int
put buf insert = do
  pos <- peekPos buf
  pos' <- insert pos
  pokePos buf pos'
  let len = pos' `minusPtr` pos
  bytesWritten_ <- peekBytesWritten buf
  pokeBytesWritten buf (bytesWritten_ + len)
  return (pos' `minusPtr` pos)
{-# INLINE put #-}

flush :: Buffer -> (Ptr Word8 -> Int -> IO a) -> IO a
flush buf f = do
  pos <- peekPos buf
  let start = bufferStart buf
  a <- f start (pos `minusPtr` start)
  pokePos buf start
  return a
{-# INLINE flush #-}

type Flush a = Ptr Word8 -> Int -> IO a

data WriteBuffer =
  WriteBuffer { _wbufBuffer :: {-# UNPACK #-} !Buffer
              , _wbufFlush  :: Flush Int
              }

withWriteBuffer :: Int -> Flush Int -> (WriteBuffer -> IO a) -> IO a
withWriteBuffer sz flsh action = do
  bracket (newBuffer sz) freeBuffer $ \buffer ->
    action (WriteBuffer buffer flsh)
{-# INLINE withWriteBuffer #-}

bytesWritten :: WriteBuffer -> IO Int
bytesWritten (WriteBuffer buffer _) = peekBytesWritten buffer
{-# INLINE bytesWritten #-}

offset :: WriteBuffer -> IO Int
offset = bytesWritten
{-# INLINE offset #-}

write :: WriteBuffer -> Int -> (Ptr Word8 -> IO (Ptr Word8)) -> IO Int
write (WriteBuffer buffer flush_) sz insert = do
  hasEnough <- hasEnoughBytes buffer sz
  bytesWritten1 <- if not hasEnough
                  then flush buffer flush_
                  else return 0
  bytesWritten2 <- put buffer insert
  return (bytesWritten1 + bytesWritten2)
{-# INLINE write #-}

writeByteString :: WriteBuffer -> ByteString.ByteString -> IO Int
writeByteString wb@(WriteBuffer buffer flush_) (ByteString.PS fop off len) = do
  withForeignPtr fop $ \op -> do
    bufSz <- size buffer
    if bufSz < len
      then do bytesWritten1 <- flush buffer flush_
              _ <- flush_ (op `plusPtr` off) len
              bytesWritten2 <- peekBytesWritten buffer
              pokeBytesWritten buffer (bytesWritten2 + len)
              return (bytesWritten1 + len)
      else write wb len (\dst -> do ByteString.memcpy dst (op `plusPtr` off) len
                                    return (dst `plusPtr` len)
                        )
{-# INLINE writeByteString #-}
