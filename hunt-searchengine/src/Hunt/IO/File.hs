{-# LANGUAGE BangPatterns #-}
module Hunt.IO.File (
    AppendFile
  , openAppendFile
  , closeAppendFile

  , appendWriter
  , bufferedWriter
  , lazyByteStringWriter
  ) where

import           Hunt.IO.Writer

import           Control.Monad
import qualified Data.ByteString          as ByteString
import           Data.ByteString.Internal (ByteString (PS))
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Lazy     as Lazy
import qualified Data.ByteString.Unsafe   as ByteString
import           Data.Word                (Word64, Word8)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified GHC.IO.Device            as FD
import qualified GHC.IO.FD                as FD
import qualified System.IO                as IO

newtype AppendFile = MkAF FD.FD

openAppendFile :: FilePath -> IO AppendFile
openAppendFile fp = do
  (fd, _) <- FD.openFile fp IO.AppendMode True
  return (MkAF fd)

closeAppendFile :: AppendFile -> IO ()
closeAppendFile (MkAF fd) = FD.close fd

-- | A writer which appends to a file. Returns the number of
--   bytes written to the file.
data AppendWriterState =
  AWS {-# UNPACK #-} !Word64

appendWriter :: AppendFile -> Writer IO ByteString Word64
appendWriter (MkAF fd) = W start step stop
  where start = pure (AWS 0)

        step (AWS n) (PS buf off len) = do
          _ <- withForeignPtr buf $ \ptr -> do
            FD.write fd (ptr `plusPtr` off) len
          return $ AWS (n + fromIntegral len)

        stop (AWS n) = pure n
{-# INLINE appendWriter #-}

-- | A writer which writes into a fixed size buffer.
--   When the buffer is full the buffer is flushed with
--   given writer.
data BufferedWriterState a =
  BWS {-# UNPACK #-} !Int {-# UNPACK #-} !(ForeignPtr Word8) !a

bufferedWriter :: Writer IO ByteString a -> Int -> Writer IO ByteString a
bufferedWriter (W h k z) bufSz = W start step stop
  where
    start = BWS 0 <$> mallocForeignPtrBytes bufSz
                  <*> h

    step (BWS offset buffer ws) inp@(PS bsPtr bsOff bsLen)
      | bsLen >= bufSz = do
          -- in case the input is bigger than
          -- the buffer size flush the buffer
          -- and directly write the input.
          let bs = ByteString.fromForeignPtr buffer 0 offset

          if offset > 0
            then do ws' <- k ws bs
                    ws'' <- k ws' (ByteString.unsafeTake bufSz inp)
                    step (BWS 0 buffer ws'') (ByteString.unsafeDrop bufSz inp)
            else do ws' <- k ws (ByteString.unsafeTake bufSz inp)
                    step (BWS 0 buffer ws') (ByteString.unsafeDrop bufSz inp)

      | n == 0 = do
          -- the buffer is full. Flush it to the writer.
          let bs = ByteString.fromForeignPtr buffer 0 offset
          ws' <- k ws bs
          step (BWS 0 buffer ws') inp

      | otherwise = do

          let
            n' :: Int
            !n' = min n bsLen

          -- we have space in the buffer to copy the input to.
          withForeignPtr buffer $ \bufPtr -> do
            withForeignPtr bsPtr $ \inpPtr -> do
              ByteString.memcpy
                (bufPtr `plusPtr` offset)
                (inpPtr `plusPtr` bsOff)
                n'

          case ByteString.unsafeDrop n' inp of
            bs | ByteString.null bs -> return $ (BWS (offset + n') buffer ws)
               | otherwise -> step (BWS (offset + n') buffer ws) bs
            where !n = bufSz - offset

    stop (BWS off buf ws)
      | off /= 0 = do
          ws' <- k ws (ByteString.fromForeignPtr buf 0 off)
          z ws'
      | otherwise = z ws
{-# INLINE bufferedWriter #-}

-- | A simple adapter for writing lazy bytestrings.
lazyByteStringWriter :: Writer IO ByteString a -> Writer IO Lazy.ByteString a
lazyByteStringWriter (W h k z) = W start step stop
  where
    start = h
    step ws lbs = foldM k ws (Lazy.toChunks lbs)
    stop ws = z ws
{-# INLINE lazyByteStringWriter #-}
