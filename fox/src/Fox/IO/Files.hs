{-# LANGUAGE BangPatterns #-}

module Fox.IO.Files (
    AppendFile
  , withAppendFile
  , append

  , MonotonicReadFile
  , openMonotonicReadFile
  , readMonotonicReadFile
  , closeMonotonicReadFile

  , RandomAccessFile
  , openRandomAccessFile
  , closeRandomAccessFile
  , readRandomAccessFile
  , seekRandomAccessFile
  ) where

import           Control.Exception
import           Data.Word
import           Foreign.Ptr
import qualified GHC.IO.Device            as FD
import qualified GHC.IO.FD                as FD
import qualified System.IO                as IO

newtype MonotonicReadFile = MkMRF FD.FD

openMonotonicReadFile :: FilePath -> IO MonotonicReadFile
openMonotonicReadFile fp = do
  (fd, _) <- FD.openFile fp IO.ReadMode True
  return (MkMRF fd)

readMonotonicReadFile :: MonotonicReadFile -> Ptr Word8 -> Int -> IO Int
readMonotonicReadFile (MkMRF fd) op n =
  FD.read fd op n

closeMonotonicReadFile :: MonotonicReadFile -> IO ()
closeMonotonicReadFile (MkMRF fd) =
  FD.close fd

newtype RandomAccessFile = MkRAF FD.FD

openRandomAccessFile :: FilePath -> IO RandomAccessFile
openRandomAccessFile fp = do
  (fd, _) <- FD.openFile fp IO.ReadMode True
  return (MkRAF fd)

closeRandomAccessFile :: RandomAccessFile -> IO ()
closeRandomAccessFile (MkRAF fd) = FD.close fd

readRandomAccessFile :: RandomAccessFile -> Int -> Ptr Word8 -> IO Int
readRandomAccessFile (MkRAF fd) n buf = FD.read fd buf n

seekRandomAccessFile :: Word64 -> RandomAccessFile -> IO ()
seekRandomAccessFile off (MkRAF fd) =
  FD.seek fd IO.AbsoluteSeek (fromIntegral off)

newtype AppendFile = MkAF FD.FD

openAppendFile :: FilePath -> IO AppendFile
openAppendFile fp = do
  (fd, _) <- FD.openFile fp IO.AppendMode True
  return (MkAF fd)

append :: AppendFile -> Ptr Word8 -> Int -> IO ()
append (MkAF fd) op sz = do
  FD.write fd op sz

closeAppendFile :: AppendFile -> IO ()
closeAppendFile (MkAF fd) = FD.close fd

withAppendFile :: FilePath -> (AppendFile -> IO a) -> IO a
withAppendFile fp action =
  bracket (openAppendFile fp) closeAppendFile action
