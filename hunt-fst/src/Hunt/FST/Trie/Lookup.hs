{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
module Hunt.FST.Trie.Lookup where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           Hunt.FST.Trie
import           Hunt.FST.Types
import           System.IO.Unsafe

--data Iter m
--  = forall s. Iter (s -> m (Transition s)) s

-- data Transition s
--  = Transition Label






lookup' :: StateRef -> Trie -> Ptr Word8 -> Ptr Word8 -> IO Word32
lookup' state (Trie trie) needle needleEnd =
  withForeignPtr ptr $ \t -> do
    lookup2 (t `plusPtr` off `plusPtr` fromIntegral state) needle needleEnd 0
  where
    ByteString.PS ptr off _ = trie

lookup :: StateRef -> Trie -> ByteString -> Word32
lookup state trie needle = unsafePerformIO $
  withForeignPtr ptr $ \n -> do
    lookup' state trie (n `plusPtr` off) (n `plusPtr` off `plusPtr` len)
  where
    ByteString.PS ptr off len = needle

lookup2 :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Word32 -> IO Word32
lookup2 fst needle needle_end hash
  | needle >= needle_end = return 0
  | otherwise            = do
    !input <- peek needle
    !flags <- peek fst

    putStrLn $ "flags: " ++ show flags

    case flags of
      0x00 -> do
        narcs <- peek (castPtr (fst `plusPtr` 1)) :: IO Word32
        putStrLn  $ "narcs: " ++ show narcs
        let
          loop 0 _ = return 0
          loop i p = do
            label <- peek p :: IO Word8
            putStrLn $ "label: " ++ show label
            case label of
              l | l == input -> do
                delta <- peek (castPtr (p `plusPtr` 1)) :: IO Word64
                putStrLn $ "delta: " ++ show delta
                lookup2 (fst `plusPtr` fromIntegral delta) (needle `plusPtr` 1) needle_end hash
                | input > l  -> return 0
                | otherwise  -> loop (i - 1) (p `plusPtr` 9)
        loop narcs (fst `plusPtr` sizeOf (undefined :: Word32) `plusPtr` 1)

      0x80 -> do
        label <- peek (fst `plusPtr` 1)

        putStrLn $ "label: " ++ show label

        if label == input
          then
          do
            delta <- peek (castPtr (fst `plusPtr` 2)) :: IO Word64
            lookup2 (fst `plusPtr` fromIntegral delta) (needle `plusPtr` 1) needle_end hash
          else return 0
      0xC0 -> do
        label <- peek (fst `plusPtr` 1)

        putStrLn $ "label: " ++ show label

        if label == input
          then lookup2 (fst `plusPtr` 2) (needle `plusPtr` 1) needle_end hash
          else return 0
      0xE0 -> do
        putStrLn $ "EOF"
        if needle_end `minusPtr` needle == 1
          then peek (castPtr (fst `plusPtr` 1))
          else return 0
      x    -> error $ "should not go here! " ++ show x
