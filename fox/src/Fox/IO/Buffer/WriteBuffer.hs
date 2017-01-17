module Fox.IO.Buffer.WriteBuffer where

import Fox.IO.Buffer (Buffer)
import qualified Fox.IO.Buffer as Buffer

data WriteBuffer =
  WriteBuffer !Buffer !Buffer.Flush

withWriteBuffer :: Int -> Buffer.Flush -> (WriteBuffer -> IO a) -> IO a
withWriteBuffer bufSz flush action = do
  Buffer.withBuffer bufSz $ \buffer -> do
    a <- action (WriteBuffer buffer flush)
    Buffer.flush buffer flush
    return a
{-# INLINE withWriteBuffer #-}

put :: WriteBuffer -> Int -> Buffer.Put -> IO ()
put (WriteBuffer buffer flush) size putter = do
  hasEnough <- Buffer.hasEnoughBytes buffer size
  if not hasEnough
    then Buffer.flush buffer flush
    else return ()
  Buffer.put buffer putter
{-# INLINE put #-}

offset :: WriteBuffer -> IO Int
offset (WriteBuffer buffer _) = Buffer.offset buffer
{-# INLINE offset #-}
