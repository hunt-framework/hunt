module Hunt.FST.Arcs where

import           Hunt.FST.Types

import           Data.Hashable
import qualified Data.List as List
import           Data.Ord
import           Data.Word

data Arc = Arc {
    arcLabel  :: !Label
  , arcWeight :: !Weight
  , arcTarget :: !StateRef
  } deriving (Eq, Show)

data Arcs = Arcs !Length !Hash ![Arc]
            deriving (Eq, Show)

instance Hashable Arc where
  hashWithSalt s (Arc l w t) =
    s `hashWithSalt` l `hashWithSalt` w `hashWithSalt` t
  {-# INLINE hashWithSalt #-}

instance Hashable Arcs where
  hashWithSalt s (Arcs sz h _) = s `hashWithSalt` sz `hashWithSalt` h
  {-# INLINE hashWithSalt #-}

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

arcsSorted :: Arcs -> [Arc]
arcsSorted (Arcs _ _ ax)
  = List.sortBy (comparing arcLabel) ax
{-# INLINE arcsSorted #-}

length :: Arcs -> Length
length (Arcs sz _ _) = sz
{-# INLINE length #-}

final :: Arc
final = Arc 0 1 0
