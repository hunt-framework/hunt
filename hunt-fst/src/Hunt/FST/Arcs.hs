module Hunt.FST.Arcs where

import           Hunt.FST.Types

import           Control.DeepSeq
import           Data.Hashable
import qualified Data.List as List
import           Prelude hiding (head)

data Arc = Arc {
    arcLabel  :: !Label
  , arcWeight :: !Weight
  , arcTarget :: !StateRef
  } deriving (Eq, Show)

type Hash = Int

type Length = Int

data Arcs = Arcs !Length !Hash ![Arc]
            deriving (Eq, Show)

instance Hashable Arc where
  hashWithSalt s (Arc l w t) =
    s `hashWithSalt` l `hashWithSalt` w `hashWithSalt` t
  {-# INLINE hashWithSalt #-}

instance Hashable Arcs where
  hashWithSalt s (Arcs sz h _) = s `hashWithSalt` sz `hashWithSalt` h
  {-# INLINE hashWithSalt #-}

instance NFData Arc where
  rnf (Arc l w t) = l `deepseq` w `deepseq` t `deepseq` ()
  {-# INLINE rnf #-}

instance NFData Arcs where
  rnf (Arcs sz h ax) = sz `deepseq` h `deepseq` ax `deepseq` ()
  {-# INLINE rnf #-}

empty :: Arcs
empty = Arcs 0 0 []
{-# INLINE empty #-}

singleton :: Arc -> Arcs
singleton arc = Arcs 1 (hash arc) [arc]
{-# INLINE singleton #-}

cons :: Arc -> Arcs -> Arcs
cons a (Arcs sz h ax) = Arcs (sz + 1) (hashWithSalt h a) (a:ax)
{-# INLINE cons #-}

arcs :: Arcs -> [Arc]
arcs (Arcs _ _ ax) = ax
{-# INLINE arcs #-}

length :: Arcs -> Length
length (Arcs sz _ _) = sz
{-# INLINE length #-}

head :: Arcs -> Arc
head (Arcs _ _ ax)
  = List.head ax
{-# INLINE head #-}

final :: Arc
final = Arc 0 1 0
