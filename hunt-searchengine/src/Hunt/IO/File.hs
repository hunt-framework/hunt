{-# LANGUAGE BangPatterns #-}
module Hunt.IO.File (
    AppendFile
  , openAppendFile
  , closeAppendFile

  , RandomAccessFile
  , openRandomAccessFile
  , closeRandomAccessFile
  , readRandomAccessFile
  , seekRandomAccessFile

  , appendWriter
  , bufferedWriter
  , lazyByteStringWriter
  ) where

import           Hunt.IO.Writer

import qualified Data.ByteString                  as ByteString
import           Data.ByteString.Builder.Internal (Builder)
import qualified Data.ByteString.Builder.Internal as Builder
import           Data.ByteString.Internal         (ByteString (PS))
import qualified Data.ByteString.Internal         as ByteString
import qualified Data.ByteString.Lazy             as Lazy
import qualified Data.ByteString.Unsafe           as ByteString
import           Data.Foldable
import           Data.Profunctor
import           Data.Word                        (Word64, Word8)
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified GHC.IO.Device                    as FD
import qualified GHC.IO.FD                        as FD
import qualified System.IO                        as IO

newtype RandomAccessFile = MkRAF FD.FD

openRandomAccessFile :: FilePath -> IO RandomAccessFile
openRandomAccessFile fp = do
  (fd, _) <- FD.openFile fp IO.ReadMode True
  return (MkRAF fd)

closeRandomAccessFile :: RandomAccessFile -> IO ()
closeRandomAccessFile (MkRAF fd) = FD.close fd

readRandomAccessFile :: Int -> RandomAccessFile -> IO ByteString
readRandomAccessFile n (MkRAF fd) =
  ByteString.createAndTrim n $ \buf -> FD.read fd buf n

seekRandomAccessFile :: Word64 -> RandomAccessFile -> IO ()
seekRandomAccessFile off (MkRAF fd) =
  FD.seek fd IO.AbsoluteSeek (fromIntegral off)

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
data BufferedWriterState a = BWS !Int !Int !(ForeignPtr Word8) !a

bufferedWriter :: Writer IO ByteString a
               -> Int
               -> Writer IO Builder.Builder a
bufferedWriter w bufSz =
  lmap Builder.runBuilder (buildStepWriter w bufSz)
{-# INLINE bufferedWriter #-}

buildStepWriter :: Writer IO ByteString a
                -> Int
                -> Writer IO (Builder.BuildStep ()) a
buildStepWriter (W wstart wstep wstop) bufSz0 = W start step stop
  where
    start = BWS 0 bufSz0 <$> mallocForeignPtrBytes bufSz0
                         <*> wstart

    step (BWS offset bufSz buffer ws) bs = withForeignPtr buffer $ \op -> do
      let
        -- done :: Ptr Word8 -> () -> IO ()
        done op' _ = return $ BWS (op' `minusPtr` op) bufSz buffer ws

        -- full :: Ptr Word8 -> Int -> Builder.BuildStep () -> IO _
        full op' minSz nextStep = do
          ws' <- wstep ws (ByteString.fromForeignPtr buffer 0 (op' `minusPtr` op))
          case () of
            _ | minSz <= bufSz -> step (BWS 0 bufSz buffer ws') nextStep
              | otherwise      -> do
                  buffer' <- mallocForeignPtrBytes minSz
                  step (BWS 0 minSz buffer' ws') nextStep

        -- insert :: Ptr Word8 -> ByteString -> Builder.BuildStep () -> IO ()
        insert op' b nextStep = do
          ws' <- wstep ws b
          step (BWS (op' `minusPtr` op) bufSz buffer ws') nextStep

      Builder.fillWithBuildStep
        bs
        done
        full
        insert
        (Builder.BufferRange (op `plusPtr` offset) (op `plusPtr` bufSz) )

    stop (BWS off _ buf ws)
      | off /= 0 = do
          ws' <- wstep ws (ByteString.fromForeignPtr buf 0 off)
          wstop ws'
      | otherwise = wstop ws
{-# INLINE buildStepWriter #-}

-- | A simple adapter for writing lazy bytestrings.
lazyByteStringWriter :: Writer IO ByteString a -> Writer IO Lazy.ByteString a
lazyByteStringWriter (W h k z) = W h step z
  where
    step ws lbs = foldlM k ws (Lazy.toChunks lbs)
{-# INLINE lazyByteStringWriter #-}
