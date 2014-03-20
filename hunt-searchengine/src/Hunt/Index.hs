{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Hunt.Index
where

import           Prelude                hiding (map)

import           GHC.Exts               (Constraint)

import           Control.DeepSeq
import qualified Data.IntSet            as IS
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap   (DocIdSet)

-- ----------------------------------------------------------------------------

-- | The index type class which needs to be implemented to be used by the 'Interpreter'.
--   The type parameter @i@ is the implementation.
--   The implementation must have a value type parameter.
class Index i where
  type IKey i v :: *          -- ^ The key type of the index.

  type IVal i v :: *          -- ^ The value type of the index.
  type IVal i v = v

  type ICon i v :: Constraint -- ^ Contraints of the index. Constraints can be on the implementation
                              --   and its value type.
  type ICon i v =  NFData v

  -- | General lookup function.
  search        :: ICon i v => TextSearchOp -> IKey i v -> i v -> [(IKey i v, IVal i v)]

  -- | Search within a range of two keys.
  lookupRange   :: ICon i v => IKey i v -> IKey i v -> i v -> [(IKey i v, IVal i v)]

  -- | Insert occurrences.
  --   This is more efficient than folding with 'insert'.
  insertList    :: ICon i v => [(IKey i v, IVal i v)] -> i v -> i v

  -- | Insert occurrences.
  insert        :: ICon i v => IKey i v -> IVal i v -> i v -> i v
  insert k v    = insertList [(k,v)]

  -- | Delete as batch job.
  --   This is more efficient than folding with 'delete'.
  deleteDocs    :: ICon i v => DocIdSet -> i v -> i v

  -- | Delete occurrences.
  delete        :: ICon i v => DocId -> i v -> i v
  delete        = deleteDocs . IS.singleton

  -- | Empty index.
  empty         :: ICon i v => i v

  -- | Convert an index to a list.
  --   Can be used for easy conversion between different index implementations.
  toList        :: ICon i v => i v -> [(IKey i v, IVal i v)]

  -- | Convert a list of key-value pairs to an index.
  fromList      :: ICon i v => [(IKey i v, IVal i v)] -> i v

  -- | Merge two indexes with a combining function.
  unionWith     :: ICon i v
                => (IVal i v -> IVal i v -> IVal i v)
                -> i v -> i v -> i v

  -- | Merge two indexes with combining functions.
  --   The second index may have another value type than the first one.
  --   Conversion and merging of the indexes is done in a single step.
  --   This is much more efficient than mapping the second index and calling 'unionWith'.
  unionWithConv :: (ICon i v, ICon i v2)
                => (v2 -> v) -> (v -> v2 -> v)
                -> i v -> i v2 -> i v

  -- TODO: non-rigid map
  -- | Map a function over the values of the index.
  map           :: ICon i v
                => (IVal i v -> IVal i v)
                -> i v -> i v
  map f = mapMaybe (Just . f)

  -- | Updates a value or deletes it if the result of the function is 'Nothing'.
  mapMaybe      :: ICon i v
                => (IVal i v -> Maybe (IVal i v))
                -> i v -> i v

  -- | Keys of the index.
  keys          :: ICon i v
                => i v -> [IKey i v]

-- ----------------------------------------------------------------------------

class Monad m => IndexM m i where
  type IKeyM     i v :: *

  type IValM     i v :: *
  type IValM     i v = v

  type IConM     i v :: Constraint
  type IConM     i v =  NFData v

  -- | Monadic version of 'search'.
  searchM      :: IConM i v => TextSearchOp -> IKeyM i v -> i v -> m [(IKeyM i v, IValM i v)]

  -- | Monadic version of 'lookupRangeM'.
  lookupRangeM :: IConM i v => IKeyM i v -> IKeyM i v -> i v -> m [(IKeyM i v, IValM i v)]

  -- | Monadic version of 'insertList'.
  insertListM  :: IConM i v => [(IKeyM i v, IValM i v)] -> i v -> m (i v)

  -- | Monadic version of 'insert'.
  insertM      :: IConM i v => IKeyM i v -> IValM i v -> i v -> m (i v)
  insertM k v i = insertListM [(k,v)] i

  -- | Monadic version of 'deleteDocs'.
  deleteDocsM  :: IConM i v => DocIdSet -> i v -> m (i v)

  -- | Monadic version of 'delete'.
  deleteM      :: IConM i v => DocId -> i v -> m (i v)
  deleteM k i  = deleteDocsM (IS.singleton k) i

  -- | Monadic version of 'empty'.
  emptyM       :: IConM i v => m (i v)

  -- | Monadic version of 'toList'.
  toListM      :: IConM i v => i v -> m [(IKeyM i v, IValM i v)]

  -- | Monadic version of 'fromList'.
  fromListM    :: IConM i v => [(IKeyM i v, IValM i v)] -> m (i v)

  -- | Monadic version of 'unionWith'.
  unionWithM   :: IConM i v
               => (IValM i v -> IValM i v -> IValM i v)
               -> i v -> i v -> m (i v)

  -- | Monadic version of 'unionWithConv'.
  unionWithConvM :: (IConM i v, Monad m, IConM i v2)
                 => (v2 -> v) -> (v -> v2 -> v)
                 -> i v -> i v2 -> m (i v)

  -- | Monadic version of 'map'.
  mapM         :: IConM i v
               => (IValM i v -> IValM i v)
               -> i v -> m (i v)
  mapM f = mapMaybeM (Just . f)

  -- | Monadic version of 'mapMaybe'.
  mapMaybeM    :: IConM i v
               => (IValM i v -> Maybe (IValM i v))
               -> i v -> m (i v)

  -- | Monadic version of 'keys'.
  keysM        :: IConM i v
               => i v -> m [IKeyM i v]

-- ----------------------------------------------------------------------------

instance (Index i, Monad m) => IndexM m i where
  type IKeyM i v             = IKey i v
  type IValM i v             = IVal i v
  type IConM i v             = ICon i v

  searchM op s i             = return $! search op s i
  lookupRangeM l u i         = return $! lookupRange l u i
  insertListM vs i           = return $! insertList vs i
  deleteDocsM ds i           = return $! deleteDocs ds i
  insertM k v i              = return $! insert k v i
  deleteM k i                = return $! delete k i
  emptyM                     = return $! empty
  toListM i                  = return $! toList i
  fromListM l                = return $! fromList l
  unionWithM f i1 i2         = return $! unionWith f i1 i2
  unionWithConvM f1 f2 i1 i2 = return $! unionWithConv f1 f2 i1 i2
  mapM f i                   = return $! map f i
  mapMaybeM f i              = return $! mapMaybe f i
  keysM i                    = return $! keys i

-- ----------------------------------------------------------------------------
