{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Hunt.FST.Trie where

import           Hunt.FST.Arcs
import qualified Hunt.FST.Arcs as Arcs
import           Hunt.FST.Register
import           Hunt.FST.Types

import           Control.Applicative hiding (empty)
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bits
import qualified Data.List as List
import           Data.Maybe
import           Data.Primitive.MutVar
import qualified Data.Traversable as Trav
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed.Mutable as MUVector
import           Data.Word (Word8, Word16, Word32)
import           Foreign.Storable

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import           GHC.Base (ord,Int(..),uncheckedShiftRL#)
import           GHC.Word (Word32(..),Word16(..),Word64(..))
# if WORD_SIZE_IN_BITS < 64
import           GHC.Word (uncheckedShiftRL64#)
# endif
#endif

data Register s a
  = Register { outputs :: !(MutVar s (MVector.MVector s a))
             , nodes   :: !(MutVar s (MUVector.MVector s Word8))
             , offset  :: !(MutVar s Int)
             , nextOut :: !(MutVar s Int)
             }

empty :: PrimMonad m => m (Register (PrimState m) a)
empty
  = do ox <- MVector.new 16
       nx <- MUVector.new 128
       ov <- newMutVar ox
       nv <- newMutVar nx
       ofv <- newMutVar 0
       no  <- newMutVar 0
       return Register { outputs = ov
                       , nodes   = nv
                       , offset  = ofv
                       , nextOut = no
                       }
{-# INLINE empty #-}

replaceOrRegister :: PrimMonad m
                  => UncompiledState a
                  -> Register (PrimState m) a
                  -> m Arc
replaceOrRegister (UncompiledState l arcs (Just o)) reg
  = do i   <- readMutVar (nextOut reg)
       insertOutput o reg
       fin <- writeFinalArc i reg
       let arcs' = Arc 0 0 fin `cons` arcs
       st  <- writeArcs arcs' reg
       return (Arc l 0 st)
replaceOrRegister (UncompiledState l arcs Nothing) reg
  = do st <- writeArcs arcs reg
       return (Arc l 0 st)
{-# INLINE replaceOrRegister #-}

writeArcs :: PrimMonad m
          => Arcs
          -> Register (PrimState m) a
          -> m StateRef
writeArcs arcs reg
  = do nx <- readMutVar (nodes reg)
       let c = MUVector.length nx
       off <- readMutVar (offset reg)
       nx' <- if off + sz >= c
              then MUVector.unsafeGrow nx (c * 2)
              else return nx
       writeMutVar (nodes reg) nx'
       n   <- case Arcs.length arcs of
                1 -> writeSimpleArc off (List.head (Arcs.arcs arcs)) reg
                _ -> writeMultipleArcs off arcs reg
       writeMutVar (offset reg) (off + n)
       return (fromIntegral off)
  where
    sz = Arcs.length arcs * 8 + 4
{-# INLINE writeArcs #-}

type Offset = Int

writeMultipleArcs :: PrimMonad m
                  => Offset
                  -> Arcs
                  -> Register (PrimState m) a
                  -> m Int
writeMultipleArcs off arcs reg
  = do v <- readMutVar (nodes reg)
       n <- go off (Arcs.arcsSorted arcs) v
       return (n - off)
  where
    go !off' []     _
      = return off'
    go !off' (a:ax) v
      = do putWord16be off'       (arcLabel a) v
           putWord32be (off' + 2) (fromIntegral off - fromIntegral (arcTarget a)) v
           go (off' + 6) ax v
{-# INLINE writeMultipleArcs #-}

writeSimpleArc :: PrimMonad m
               => Offset
               -> Arc
               -> Register (PrimState m) a
               -> m Int
writeSimpleArc off arc reg
  | isPrevious = do writeSingleNextArc off arc reg
                    return (sizeOf (undefined :: Word32))
  | otherwise  = do writeSingleArc off arc reg
                    return (sizeOf (undefined :: Word32) + sizeOf (undefined :: Word32))
  where
    isPrevious = off - fromIntegral (arcTarget arc) == 0
{-# INLINE writeSimpleArc #-}

writeSingleArc :: PrimMonad m
               => Offset
               -> Arc
               -> Register (PrimState m) a
               -> m ()
writeSingleArc off arc reg
  = do v <- readMutVar (nodes reg)
       putWord32be off       word v
       putWord32be (off + 4) (fromIntegral off - fromIntegral (arcTarget arc)) v
       return ()
  where
    word :: Word32
    word = 0xC0000000 .|. (fromIntegral (arcLabel arc) .&. 0x0000FFFF )
{-# INLINE writeSingleArc #-}

writeSingleNextArc :: PrimMonad m
                   => Offset
                   -> Arc
                   -> Register (PrimState m) a
                   -> m ()
writeSingleNextArc off arc reg
  = do v <- readMutVar (nodes reg)
       putWord32be off word v
  where
    word :: Word32
    word = 0xC0000000 .|. (fromIntegral (arcLabel arc) .&. 0x0000FFFF )
{-# INLINE writeSingleNextArc #-}

writeFinalArc :: PrimMonad m
              => Int
              -> Register (PrimState m) a
              -> m StateRef
writeFinalArc a reg
  = do v   <- readMutVar (nodes reg)
       off <- readMutVar (offset reg)
       putWord16be off 0xF000 v
       putWord32be (off + 2) (fromIntegral a) v
       return 6
{-# INLINE writeFinalArc #-}

insertOutput :: PrimMonad m
             => a
             -> Register (PrimState m) a
             -> m ()
insertOutput a r
  = do i <- readMutVar (nextOut r)
       o <- readMutVar (outputs r)
       let c = MVector.length o
       o' <- if i >= c
             then MVector.unsafeGrow o c
             else return o
       MVector.unsafeWrite o' i a
       writeMutVar (outputs r) o'
       writeMutVar (nextOut r) (succ i)
{-# INLINE insertOutput #-}

putWord16be :: PrimMonad m
            => Offset
            -> Word16
            -> MUVector.MVector (PrimState m) Word8
            -> m ()
putWord16be off w v
  = do MUVector.unsafeWrite v off       (fromIntegral (shiftr_w16 w 8) :: Word8)
       MUVector.unsafeWrite v (off + 1) (fromIntegral w                :: Word8)
{-# INLINE putWord16be #-}

putWord32be :: PrimMonad m
            => Offset
            -> Word32
            -> MUVector.MVector (PrimState m) Word8
            -> m ()
putWord32be off w v
  = do MUVector.unsafeWrite v off       (fromIntegral (shiftr_w32 w 24) :: Word8)
       MUVector.unsafeWrite v (off + 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
       MUVector.unsafeWrite v (off + 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
       MUVector.unsafeWrite v (off + 3) (fromIntegral (w)               :: Word8)
{-# INLINE putWord32be #-}
------------------------------------------------------------------------
-- Unchecked shifts

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

# if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)
# else
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
# endif

#else
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif
