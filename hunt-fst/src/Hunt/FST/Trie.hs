module Hunt.FST.Trie where

import           Hunt.FST.Arcs
import           Hunt.FST.Register
import           Hunt.FST.Types

import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector.Unboxed.Mutable as MUVector
import           Data.Word (Word8)

data Register s a
  = Register { outputs :: !(MutVar s (MVector.MVector s a))
             , nodes   :: !(MutVar s (MUVector.MVector s Word8))
             , nextOut :: !(MutVar s Int)
             }

replaceOrRegister :: PrimMonad m
                  => UncompiledState a
                  -> Register (PrimState m) a
                  -> m Arc
replaceOrRegister uc@(UncompiledState _ _ (Just o)) reg
  = do insertOutput o reg
       replaceOrRegister' uc True reg
replaceOrRegister uc reg
  = replaceOrRegister' uc False reg

replaceOrRegister' :: PrimMonad m
                   => UncompiledState a
                   -> Bool
                   -> Register (PrimState m) a
                   -> m Arc
replaceOrRegister' (UncompiledState l a _) isFinal reg
  = do undefined

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
