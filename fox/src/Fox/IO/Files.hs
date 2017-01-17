{-# LANGUAGE BangPatterns #-}

module Fox.IO.Files (
    AppendFile
  , withAppendFile
  , append

  , RandomAccessFile
  , openRandomAccessFile
  , closeRandomAccessFile
  , readRandomAccessFile
  , seekRandomAccessFile

  ) where

import           Control.Exception
import           Data.ByteString.Internal (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.Word
import           Foreign.Ptr
import qualified GHC.IO.Device            as FD
import qualified GHC.IO.FD                as FD
import qualified System.IO                as IO

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

read :: Int -> Ptr Word8 -> RandomAccessFile -> IO Int
read sz op (MkRAF fd) = FD.read fd op sz

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
