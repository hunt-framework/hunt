{-# LANGUAGE RankNTypes    #-}
module Fox.IO.Buffer (
    Buffer
  , withBuffer
  , hasEnoughBytes
  , offset

  , Get
  , get

  , Put
  , put

  , Flush
  , flush

  , FillBuffer
  , fill
  ) where

import           GHC.Exts

import           Control.Exception
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Prelude                  hiding (read)

-- | An abstraction for flushing a buffer
type Flush = Ptr Word8 -> Int -> IO ()

-- | An abstraction for filling a buffer.
-- Start of buffer, current position in buffer and buffer size.
type FillBuffer = Ptr Word8 -> Ptr Word8 -> Int -> IO ()

-- | Writing some bytes into the buffer starting
-- from its current cursor.
type Put = Ptr Word8 -> IO (Ptr Word8)

-- | Read bytes from buffer starting from its current cursor.
type Get a = Ptr Word8 -> IO (a, Ptr Word8)

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

peekOffset :: Buffer -> IO Int
peekOffset (Buffer op) =
  peek (castPtr (op `plusPtr` (2 * sizeOfPtr)))
{-# INLINE peekOffset #-}

pokeOffset :: Buffer -> Int -> IO ()
pokeOffset (Buffer op) n =
  poke (castPtr (op `plusPtr` (2 * sizeOfPtr))) n
{-# INLINE pokeOffset #-}

incrOffset :: Buffer -> Int -> IO ()
incrOffset buf n = do
  x <- peekOffset buf
  pokeOffset buf (x + n)
{-# INLINE incrOffset #-}

newBuffer :: Int -> IO Buffer
newBuffer bufSize = do
  -- we use cs malloc here to avoid
  -- copying this buffer during GCs.
  buf <- mallocBytes (bufSize + 2 * sizeOfPtr + 1 * sizeOfInt)
  pokePos (Buffer buf) (bufferStart (Buffer buf))
  pokeEnd (Buffer buf) (bufferStart (Buffer buf)  `plusPtr` bufSize)
  pokeOffset (Buffer buf) 0
  return (Buffer buf)

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer bufSize action =
  bracket (newBuffer bufSize) freeBuffer action

freeBuffer :: Buffer -> IO ()
freeBuffer (Buffer buf) = free buf

hasEnoughBytes :: Buffer -> Int -> IO Bool
hasEnoughBytes buf n = do
  pos <- peekPos buf
  end <- peekEnd buf
  return $ end `minusPtr` pos >= n
{-# INLINE hasEnoughBytes #-}

offset :: Buffer -> IO Int
offset buf = peekOffset buf
{-# INLINE offset #-}

put :: Buffer -> Put -> IO ()
put buf insert = do
  pos <- peekPos buf
  pos' <- insert pos
  pokePos buf pos'
  let len = pos' `minusPtr` pos
  incrOffset buf len
{-# INLINE put #-}

get :: Buffer
    -> Get a
    -> IO a
get buf get_ = do
  pos <- peekPos buf
  r <- get_ pos
  case r of
    (a, pos') -> do
      pokePos buf pos'
      let len = pos' `minusPtr` pos
      incrOffset buf len
      return $! a
{-# INLINE get #-}

fill :: Buffer -> FillBuffer -> IO ()
fill buf filler = do
  pos <- peekPos buf
  end <- peekEnd buf
  let start = bufferStart buf
  pokePos buf start
  filler start pos (end `minusPtr` start)
{-# INLINE fill #-}

flush :: Buffer -> Flush -> IO ()
flush buf f = do
  pos <- peekPos buf
  let start = bufferStart buf
  if (start /= pos)
    then do f start (pos `minusPtr` start)
            pokePos buf start
    else return ()
{-# INLINE flush #-}
