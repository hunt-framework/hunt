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

import           Control.Arrow          (second)

import qualified Data.List              as L

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdSet   (DocIdSet)
import qualified Hunt.Common.DocIdSet   as DS
import           Hunt.Common.IntermediateValue

-- ------------------------------------------------------------

-- | The index type class which needs to be implemented to be used by the 'Interpreter'.
--   The type parameter @i@ is the implementation.
--   The implementation must have a value type parameter.
class Index i where
  -- | The key type of the index.
  type IKey i :: *
  type IVal i :: *

  type ICon i :: Constraint
  type ICon i = ()

  -- | General lookup function.
  search        :: (IndexValue (IVal i), ICon i) => TextSearchOp -> IKey i -> i -> [(IKey i, IntermediateValue)]

  searchSc      :: (IndexValue (IVal i), ICon i) => TextSearchOp -> IKey i -> i -> [(IKey i, (Score, IntermediateValue))]
  searchSc op k ix = addDefScore $ search op k ix

  -- | Search within a range of two keys.
  lookupRange   :: (IndexValue (IVal i), ICon i) => IKey i -> IKey i -> i -> [(IKey i, IntermediateValue)]

  lookupRangeSc :: (IndexValue (IVal i), ICon i) => IKey i -> IKey i -> i -> [(IKey i, (Score, IntermediateValue))]
  lookupRangeSc k1 k2 ix
                = addDefScore $ lookupRange k1 k2 ix

  -- | Insert occurrences.
  --   This is more efficient than folding with 'insert'.
  insertList    :: (IndexValue (IVal i), ICon i) =>
                   [(IKey i, IntermediateValue)] -> i -> i

  -- | Insert occurrences.
  insert        :: (IndexValue (IVal i), ICon i) =>
                   IKey i -> IntermediateValue -> i -> i
  insert   k v  = insertList [(k,v)]

  -- | Delete as batch job.
  --   This is more efficient than folding with 'delete'.
  deleteDocs    :: (IndexValue (IVal i), ICon i) => DocIdSet -> i -> i

  -- | Delete occurrences.
  delete        :: (IndexValue (IVal i), ICon i) => DocId -> i -> i
  delete        = deleteDocs . DS.singleton

  -- | Empty index.
  empty         :: (IndexValue (IVal i), ICon i) => i

  -- | Convert an index to a list.
  --   Can be used for easy conversion between different index implementations.
  toList        :: (IndexValue (IVal i), ICon i) => i -> [(IKey i, IntermediateValue)]

  -- | Convert a list of key-value pairs to an index.
  fromList      :: (IndexValue (IVal i), ICon i) => [(IKey i, IntermediateValue)] -> i

  -- | Merge two indexes with a combining function.
  unionWith     :: (IndexValue (IVal i), ICon i)
                => (IVal i -> IVal i -> IVal i)
                -> i -> i -> i

  --   Merge two indexes with combining functions.
  --   The second index may have another value type than the first one.
  --   Conversion and merging of the indexes is done in a single step.
  --   This is much more efficient than mapping the second index and calling 'unionWith'.
  --  unionWithConv :: ((IndexValue (IVal i), ICon i), (IndexValue (IVal i), ICon i)2)
  --                => IVal i)2 -> IVal i) -> (v -> v2 -> IVal i)
  --                -> i -> i2 -> i

  -- TODO: non-rigid map
  -- | Map a function over the values of the index.
  map           :: (IndexValue (IVal i), ICon i)
                => (IVal i -> IVal i)
                -> i -> i
  map f = mapMaybe (Just . f)

  -- | Updates a value or deletes it if the result of the function is 'Nothing'.
  mapMaybe      :: (IndexValue (IVal i), ICon i)
                => (IVal i -> Maybe (IVal i))
                -> i -> i

  -- | Keys of the index.
  keys          :: (IndexValue (IVal i), ICon i)
                => i -> [IKey i]

-- ------------------------------------------------------------

-- | Monadic version of 'Index'.
--   'Index' instances are automatically instance of this type class.
class Monad m => IndexM m i where
  -- | The key type of the index.
  type IKeyM     i :: *

  -- | The value type of the index.
  type IValM     i :: *

  type IConM     i :: Constraint
  type IConM     i = ()

  -- | Monadic version of 'search'.
  searchM      :: (IndexValue (IValM i), IConM i) => TextSearchOp -> IKeyM i -> i -> m [(IKeyM i, IntermediateValue)]

  -- | Monadic version of 'search' with (default) scoring.
  searchMSc     :: (IndexValue (IValM i), IConM i) => TextSearchOp -> IKeyM i -> i -> m [(IKeyM i, (Score, IntermediateValue))]
  searchMSc op k ix
                = searchM op k ix >>= return . addDefScore

  -- | Monadic version of 'lookupRangeM'.
  lookupRangeM :: (IndexValue (IValM i), IConM i) => IKeyM i -> IKeyM i -> i -> m [(IKeyM i, IntermediateValue)]

  lookupRangeMSc :: (IndexValue (IValM i), IConM i) => IKeyM i -> IKeyM i -> i -> m [(IKeyM i, (Score, IntermediateValue))]
  lookupRangeMSc k1 k2 ix
                = lookupRangeM k1 k2 ix >>= return . addDefScore

  -- | Monadic version of 'insertList'.
  insertListM  :: (IndexValue (IValM i), IConM i) =>
                  [(IKeyM i, IntermediateValue)] -> i -> m (i)

  -- | Monadic version of 'insert'.
  insertM      :: (IndexValue (IValM i), IConM i) =>
                  IKeyM i -> IntermediateValue -> i -> m (i)
  insertM k v  = insertListM [(k,v)]

  -- | Monadic version of 'deleteDocs'.
  deleteDocsM  :: (IndexValue (IValM i), IConM i) => DocIdSet -> i -> m (i)

  -- | Monadic version of 'delete'.
  deleteM      :: (IndexValue (IValM i), IConM i) => DocId -> i -> m (i)
  deleteM k i  = deleteDocsM (DS.singleton k) i

  -- | Monadic version of 'empty'.
  emptyM       :: (IndexValue (IValM i), IConM i) => m (i)

  -- | Monadic version of 'toList'.
  toListM      :: (IndexValue (IValM i), IConM i) => i -> m [(IKeyM i, IntermediateValue)]

  -- | Monadic version of 'fromList'.
  fromListM    :: (IndexValue (IValM i), IConM i) => [(IKeyM i, IntermediateValue)] -> m (i)

  -- | Monadic version of 'unionWith'.
  unionWithM   :: (IndexValue (IValM i), IConM i) =>
                  (IValM i -> IValM i -> IValM i) ->
                  i -> i -> m (i)

  --  Monadic version of 'unionWithConv'.
  -- unionWithConvM :: ((IndexValue (IValM i), IConM i), Monad m, (IndexValue (IValM i), IConM i)2)
  --               => IVal i)2 -> IVal i) -> (v -> v2 -> IVal i)
  --               -> i -> i2 -> m (i)

  -- | Monadic version of 'map'.
  mapM         :: (IndexValue (IValM i), IConM i)
               => (IValM i -> IValM i)
               -> i -> m (i)
  mapM f = mapMaybeM (Just . f)

  -- | Monadic version of 'mapMaybe'.
  mapMaybeM    :: (IndexValue (IValM i), IConM i)
               => (IValM i -> Maybe (IValM i))
               -> i -> m (i)

  -- | Monadic version of 'keys'.
  keysM        :: (IndexValue (IValM i), IConM i)
               => i -> m [IKeyM i]

-- ------------------------------------------------------------

instance (Index i, Monad m) => IndexM m i where
  type IKeyM i             = IKey i
  type IValM i             = IVal i
  type IConM i             = ICon i

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
