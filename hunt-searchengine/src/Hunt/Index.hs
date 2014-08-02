{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

-- ----------------------------------------------------------------------------
{- |
  The index interface.
-}
-- ----------------------------------------------------------------------------


module Hunt.Index
where

import           Prelude                hiding (map)

import           GHC.Exts               (Constraint)

import           Control.DeepSeq
import           Control.Arrow          (second)

import           Data.Binary            (Binary (..))
import qualified Data.List              as L

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences (Occurrences)
import qualified Hunt.Common.Occurrences as Occ
import           Hunt.Common.DocId
import           Hunt.Common.DocIdSet   (DocIdSet)
import qualified Hunt.Common.DocIdSet   as DS


-- ------------------------------------------------------------

class (Binary x, NFData x) => IndexValue x where
  toOccurrences   :: x -> Occurrences
  fromOccurrences :: Occurrences -> x
  mergeValues     :: x -> x -> x
  diffValues      :: DocIdSet -> x -> Maybe x

instance IndexValue Occurrences where
  toOccurrences   = id
  fromOccurrences = id
  mergeValues     = Occ.merge
  diffValues s m  = let z = Occ.diffWithSet m s
                    in
                      if Occ.null z
                      then Nothing
                      else Just z

-- ------------------------------------------------------------

-- | The index type class which needs to be implemented to be used by the 'Interpreter'.
--   The type parameter @i@ is the implementation.
--   The implementation must have a value type parameter.
class Index i where
  -- | The key type of the index.
  type IKey i

  type IVal i v
  type IVal i v = v

  -- | Contraints of the index. Constraints can be on the implementation and its value type.
  type ICon i v :: Constraint
  type ICon i v =  IndexValue v

  -- | General lookup function.
  search        :: (IndexValue (v), ICon i v) => TextSearchOp -> IKey i v -> i v -> [(IKey i v, v)]

  searchSc      :: (IndexValue (v), ICon i v) => TextSearchOp -> IKey i v -> i v -> [(IKey i v, (Score, v))]
  searchSc op k ix = addDefScore $ search op k ix

  -- | Search within a range of two keys.
  lookupRange   :: (IndexValue (v), ICon i v) => IKey i v -> IKey i v -> i v -> [(IKey i v, v)]

  lookupRangeSc :: (IndexValue (v), ICon i v) => IKey i v -> IKey i v -> i v -> [(IKey i v, (Score, v))]
  lookupRangeSc k1 k2 ix
                = addDefScore $ lookupRange k1 k2 ix

  -- | Insert occurrences.
  --   This is more efficient than folding with 'insert'.
  insertList    :: (IndexValue (v), ICon i v) =>
                   [(IKey i v, v)] -> i v -> i v

  -- | Insert occurrences.
  insert        :: (IndexValue (v), ICon i v) =>
                   IKey i v -> v -> i v -> i v
  insert   k v  = insertList [(k,v)]

  -- | Delete as batch job.
  --   This is more efficient than folding with 'delete'.
  deleteDocs    :: ICon i v => DocIdSet -> i v -> i v

  -- | Delete occurrences.
  delete        :: ICon i v => DocId -> i v -> i v
  delete        = deleteDocs . DS.singleton

  -- | Empty index.
  empty         :: ICon i v => i v

  -- | Convert an index to a list.
  --   Can be used for easy conversion between different index implementations.
  toList        :: (IndexValue (v), ICon i v) => i v -> [(IKey i v, v)]

  -- | Convert a list of key-value pairs to an index.
  fromList      :: (IndexValue (v), ICon i v) => [(IKey i v, v)] -> i v

  -- | Merge two indexes with a combining function.
  unionWith     :: (IndexValue (v), ICon i v)
                => (v -> v -> v)
                -> i v -> i v -> i v

  --   Merge two indexes with combining functions.
  --   The second index may have another value type than the first one.
  --   Conversion and merging of the indexes is done in a single step.
  --   This is much more efficient than mapping the second index and calling 'unionWith'.
  --  unionWithConv :: (ICon i v, ICon i v2)
  --                => (v2 -> v) -> (v -> v2 -> v)
  --                -> i v -> i v2 -> i v

  -- TODO: non-rigid map
  -- | Map a function over the values of the index.
  map           :: (IndexValue (v), ICon i v)
                => (v -> v)
                -> i v -> i v
  map f = mapMaybe (Just . f)

  -- | Updates a value or deletes it if the result of the function is 'Nothing'.
  mapMaybe      :: (IndexValue (v), ICon i v)
                => (v -> Maybe (v))
                -> i v -> i v

  -- | Keys of the index.
  keys          :: ICon i v
                => i v -> [IKey i v]

-- ------------------------------------------------------------

-- | Monadic version of 'Index'.
--   'Index' instances are automatically instance of this type class.
class Monad m => IndexM m i where
  -- | The key type of the index.
  type IKeyM     i v :: *

  -- | The value type of the index.
  type IValM     i v :: *
  type IValM     i v = v

  -- | Contraints of the index. Constraints can be on the implementation and its value type.
  type IConM     i v :: Constraint
  type IConM     i v =  NFData v

  -- | Monadic version of 'search'.
  searchM      :: (IndexValue (IValM i v), IConM i v) => TextSearchOp -> IKeyM i v -> i v -> m [(IKeyM i v, IValM i v)]

  -- | Monadic version of 'search' with (default) scoring.
  searchMSc     :: (IndexValue (IValM i v), IConM i v) => TextSearchOp -> IKeyM i v -> i v -> m [(IKeyM i v, (Score, IValM i v))]
  searchMSc op k ix
                = searchM op k ix >>= return . addDefScore

  -- | Monadic version of 'lookupRangeM'.
  lookupRangeM :: (IndexValue (IValM i v), IConM i v) => IKeyM i v -> IKeyM i v -> i v -> m [(IKeyM i v, IValM i v)]

  lookupRangeMSc :: (IndexValue (IValM i v), IConM i v) => IKeyM i v -> IKeyM i v -> i v -> m [(IKeyM i v, (Score, IValM i v))]
  lookupRangeMSc k1 k2 ix
                = lookupRangeM k1 k2 ix >>= return . addDefScore

  -- | Monadic version of 'insertList'.
  insertListM  :: (IndexValue (IValM i v), IConM i v) =>
                  [(IKeyM i v, IValM i v)] -> i v -> m (i v)

  -- | Monadic version of 'insert'.
  insertM      :: (IndexValue (IValM i v), IConM i v) =>
                  IKeyM i v -> IValM i v -> i v -> m (i v)
  insertM k v  = insertListM [(k,v)]

  -- | Monadic version of 'deleteDocs'.
  deleteDocsM  :: (IndexValue (IValM i v), IConM i v) => DocIdSet -> i v -> m (i v)

  -- | Monadic version of 'delete'.
  deleteM      :: (IndexValue (IValM i v), IConM i v) => DocId -> i v -> m (i v)
  deleteM k i  = deleteDocsM (DS.singleton k) i

  -- | Monadic version of 'empty'.
  emptyM       :: (IndexValue (IValM i v), IConM i v) => m (i v)

  -- | Monadic version of 'toList'.
  toListM      :: (IndexValue (IValM i v), IConM i v) => i v -> m [(IKeyM i v, IValM i v)]

  -- | Monadic version of 'fromList'.
  fromListM    :: (IndexValue (IValM i v), IConM i v) => [(IKeyM i v, IValM i v)] -> m (i v)

  -- | Monadic version of 'unionWith'.
  unionWithM   :: (IndexValue (IValM i v), IConM i v) =>
                  (IValM i v -> IValM i v -> IValM i v) ->
                  i v -> i v -> m (i v)

  --  Monadic version of 'unionWithConv'.
  -- unionWithConvM :: ((IndexValue (IValM i v), IConM i v), Monad m, (IndexValue (IValM i v), IConM i v)2)
  --               => (v2 -> v) -> (v -> v2 -> v)
  --               -> i v -> i v2 -> m (i v)

  -- | Monadic version of 'map'.
  mapM         :: (IndexValue (IValM i v), IConM i v)
               => (IValM i v -> IValM i v)
               -> i v -> m (i v)
  mapM f = mapMaybeM (Just . f)

  -- | Monadic version of 'mapMaybe'.
  mapMaybeM    :: (IndexValue (IValM i v), IConM i v)
               => (IValM i v -> Maybe (IValM i v))
               -> i v -> m (i v)

  -- | Monadic version of 'keys'.
  keysM        :: (IndexValue (IValM i v), IConM i v)
               => i v -> m [IKeyM i v]

-- ------------------------------------------------------------

instance (Index i, Monad m) => IndexM m i where
  type IKeyM i v             = IKey i v
  type IValM i v             = v
  type IConM i v             = ICon i v

  searchM   op s i           = return $  search op s i
  searchMSc op s i           = return $  searchSc op s i
  lookupRangeM   l u i       = return $  lookupRange   l u i
  lookupRangeMSc l u i       = return $  lookupRangeSc l u i
  insertListM  vs i          = return $! insertList vs i
  deleteDocsM ds i           = return $! deleteDocs ds i
  insertM k v i              = return $! insert k v i
  deleteM k i                = return $! delete k i
  emptyM                     = return $! empty
  toListM i                  = return $  toList i
  fromListM l                = return $! fromList l
  unionWithM f i1 i2         = return $! unionWith f i1 i2
--  unionWithConvM f1 f2 i1 i2 = return $! unionWithConv f1 f2 i1 i2
  mapM f i                   = return $! map f i
  mapMaybeM f i              = return $! mapMaybe f i
  keysM i                    = return $  keys i

-- ------------------------------------------------------------

addDefScore :: [(a, b)] -> [(a, (Score, b))]
addDefScore = L.map (second (\ x -> (defScore, x)))

-- ------------------------------------------------------------
