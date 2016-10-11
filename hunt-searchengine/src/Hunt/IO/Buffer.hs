{-# LANGUAGE RecordWildCards #-}
module Hunt.IO.Buffer where

import           Data.Primitive.Addr
import           Data.Primitive.PrimRef
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           GHC.Exts               (Ptr (..))

-- | A not-thread safe mutable buffer.
data Buffer =
  Buffer { bufStart :: !(Ptr Word8)
         , bufEnd   :: !(Ptr Word8)
         , bufPos   :: !(PrimRef (PrimState IO) Addr)
         }

type Flush a = Ptr Word8 -> Int -> IO a

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer sz f = do
  -- N.B 'mallocForeignptrBytes' of
  -- course uses pinned memory
  fop <- mallocForeignPtrBytes sz
  withForeignPtr fop $ \op@(Ptr p) -> do
    mop <- newPrimRef (Addr p)
    f Buffer { bufStart = op
             , bufEnd   = op `plusPtr` sz
             , bufPos   = mop
             }
{-# INLINE withBuffer #-}

reset :: Buffer -> IO ()
reset buf = do
  let p = case bufStart buf of
            Ptr op -> Addr op
  writePrimRef (bufPos buf) p
{-# INLINE reset #-}

null :: Buffer -> IO Bool
null buf = do
  Addr pos <- readPrimRef (bufPos buf)
  return $ bufStart buf == Ptr pos
{-# INLINE null #-}

hasEnoughBytes :: Buffer -> Int -> IO Bool
hasEnoughBytes buf n = do
  Addr pos <- readPrimRef (bufPos buf)
  return $ bufEnd buf `minusPtr` Ptr pos >= n
{-# INLINE hasEnoughBytes #-}

flush :: Buffer -> Flush a -> IO a
flush buf f = do
  Addr pos <- readPrimRef (bufPos buf)
  a <- f (bufStart buf) (Ptr pos `minusPtr` bufStart buf)
  let p = case bufStart buf of
            Ptr op -> Addr op
  writePrimRef (bufPos buf) p
  return a
{-# INLINE flush #-}

put :: Buffer
     -> (Ptr Word8 -> IO (Ptr Word8))
     -> IO Int
put buf insert = do
  Addr pos <- readPrimRef (bufPos buf)
  Ptr pos' <- insert (Ptr pos)
  writePrimRef (bufPos buf) (Addr pos')
  return (Ptr pos' `minusPtr` Ptr pos)
{-# INLINE put #-}
