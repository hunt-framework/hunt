{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=500 -O0 #-}
module Hunt.FST.Compile where

import           Prelude hiding (head, tail)

import           Hunt.FST.Trie
import           Hunt.FST.Arcs (Arc)
import qualified Hunt.FST.Arcs as Arcs
import           Hunt.FST.Register
import           Hunt.FST.Types

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Text (Text)
import qualified Data.Text.Array as Array
import qualified Data.Text.Internal as Text
import Data.Vector.Fusion.Stream (liftStream)
import           Data.Vector.Fusion.Stream.Monadic as Stream
import           Data.Vector.Fusion.Stream.Size as Stream
import qualified Data.Vector.Internal.Check as Ck
import Data.Vector.Generic (stream, unstream)
import Data.Vector.Generic.Mutable (mstream, munstream)
import Unsafe.Coerce
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector as Vector

import           Data.Word (Word8)
import           Data.Primitive.MutVar
import qualified Data.Vector.Unboxed as UVector

import Debug.Trace


#define ERROR (Ck.error __FILE__ __LINE__)
#define EMPTY_STREAM (\sx123 -> ERROR sx123 "emptyStream")

mkUncompiledState ::  PrimMonad m => Text -> a -> Stream m (UncompiledState a)
mkUncompiledState (Text.Text arr off len) o
  = Stream next off (Exact len)
  where
    {-# INLINE next #-}
    !end = off + len
    next !i
      | i >= end     = return Done
      | i + 1 == end = return $ Yield (UncompiledState n Arcs.empty (Just o)) (i + 1)
      | otherwise    = return $ Yield (UncompiledState n Arcs.empty Nothing) (i + 1)
      where
        n = Array.unsafeIndex arr i
{-# INLINE mkUncompiledState #-}

data CS a b
  = CS0 !b
  | CS1 !(UncompiledState a) !b

compileSuffix :: PrimMonad m
              => Register (PrimState m) a
              -> Stream m (UncompiledState a)
              -> m Arc
compileSuffix register (Stream next1 x1 _)
  = loop0 SPEC (CS0 x1)
  where
    loop0 !sPEC (CS0 !s)
      = do r <- next1 s
           case r of
            Yield (UncompiledState !l !a !o) !s' -> do
              !arc <- loop1 SPEC (CS1 (UncompiledState l a o) s')
              arc `seq` replaceOrRegister (UncompiledState l (arc `Arcs.cons` a) o) register
            Skip !s' -> loop0 SPEC (CS0 s')
            Done    -> EMPTY_STREAM "compileStream"
    loop1 !sPEC (CS1 !uc !s)
      = do r <- next1 s
           case r of
            Yield uc'@(UncompiledState !l !a !o) !s' -> do
              !arc <- loop1 SPEC (CS1 uc' s')
              arc `seq` replaceOrRegister (UncompiledState l (arc `Arcs.cons` a) o) register
            Skip !s' -> loop1 SPEC (CS1 uc s')
            Done    -> replaceOrRegister uc register
{-# INLINE compileSuffix #-}

data C a b c
  = C0 !b !c
  | C1 !(UncompiledState a) !b !c
  | C2 !c

compile :: PrimMonad m
        => Register (PrimState m) a
        -> Stream m (UncompiledState a)
        -> Stream m (UncompiledState a)
        -> Stream m (UncompiledState a)
compile register _old@(Stream next1 x1 n1) _new@(Stream next2 x2 n2)
  = Stream next (C0 x1 x2)  (n1 + n2)
  where
    {-# INLINE next #-}
    next (C0 !s1 !s2)
      = do r1 <- next1 s1
           case r1 of
            Yield x s1' -> return $ Skip (C1 x s1' s2)
            Skip s1'    -> return $ Skip (C0 s1' s2)
            Done        -> EMPTY_STREAM "compile: empty stream"
    next (C1 !prev !s1 !s2)
      = do r1 <- next1 s1
           case r1 of
            Yield x s1' -> do
              r2 <- next2 s2
              case r2 of
               Yield y s2'
                 | ucLabel x == ucLabel y -> return $ Yield prev (C1 x s1' s2')
                 | otherwise              -> do
                     !arc <- compileSuffix register (Stream next1 s1 n1)
                     return $ Yield (prev {ucArcs = arc `Arcs.cons` ucArcs prev }) (C2 s2)
               Done        -> EMPTY_STREAM "compile: not in lexicographic order"
               Skip s2'    -> return $ Skip (C1 prev s1 s2')
            Done        -> return $ Yield prev (C2 s2)
            Skip s1'    -> return $ Skip (C1 prev s1' s2)
    next (C2 !s2)
      = do r2 <- next2 s2
           case r2 of
            Yield x s2' -> return $ Yield x (C2 s2')
            Done        -> return   Done
            Skip s2'    -> return $ Skip (C2 s2')
{-# INLINE compile #-}

compileList :: PrimMonad m
            => Register (PrimState m) a
            -> [(Text, a)]
            -> m StateRef
compileList register
  = do loop rootState
  where
    loop path []
      = do rootArc <- compileSuffix register (liftStream (stream path))
           return (Arcs.arcTarget rootArc)
    loop path ((w, a):ex)
      = do buf  <- munstream path'
           buf' <- Vector.unsafeFreeze buf
           loop buf' ex
      where
        path' = compile register (liftStream (stream path)) (mkUncompiledState w a)
    rootState
      = Vector.singleton (UncompiledState maxBound Arcs.empty Nothing)
{-# INLINE compileList #-}

compileList' :: [(Text, a)] -> UVector.Vector Word8
compileList' wx
  = runST $ do r <- Hunt.FST.Trie.empty
               s <- compileList r wx
               nx <- readMutVar (nodes r)
               ix <- UVector.unsafeFreeze nx
               return ix
{-# INLINE compileList' #-}
