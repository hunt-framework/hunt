{-# LANGUAGE RecordWildCards #-}
module Hunt.IO.Buffer where

import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr

data Buffer =
  Buffer { bufStart :: !(Ptr Word8)
         , bufEnd   :: !(Ptr Word8)
         , bufPos   :: !(Ptr Word8)
         }

type Flush a = Ptr Word8 -> Int -> IO a

withBuffer :: Int -> (Buffer -> IO a) -> IO a
withBuffer sz f = do
  fop <- mallocForeignPtrBytes sz
  withForeignPtr fop $ \op -> do
    f Buffer { bufStart = op
             , bufEnd   = op `plusPtr` sz
             , bufPos   = op
             }
{-# INLINE withBuffer #-}

reset :: Buffer -> Buffer
reset buf = buf { bufPos = bufStart buf }
{-# INLINE reset #-}

null :: Buffer -> Bool
null buf = bufStart buf == bufPos buf
{-# INLINE null #-}

hasEnoughBytes :: Buffer -> Int -> Bool
hasEnoughBytes buf n =
  bufEnd buf `minusPtr` bufPos buf >= n
{-# INLINE hasEnoughBytes #-}

flush :: Flush a -> Buffer -> IO a
flush f buf = f (bufStart buf) (bufPos buf `minusPtr` bufStart buf)
{-# INLINE flush #-}

put :: Buffer
    -> (Ptr Word8 -> IO (Ptr Word8))
    -> IO Buffer
put buf insert = do
  start' <- insert (bufPos buf)
  return buf { bufPos = start' }
{-# INLINE put #-}
