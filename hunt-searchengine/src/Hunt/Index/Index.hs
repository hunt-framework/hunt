{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Hunt.Index.Index
where

import           Prelude                        hiding (map)

import           GHC.Exts                       (Constraint)

import           Control.DeepSeq
import qualified Data.IntSet                    as IS
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdMap           (DocIdSet)
import           Hunt.Common.DocId

-- ----------------------------------------------------------------------------

class Monad m => IndexM m i where
  type IKeyM     i v :: *

  type IValM     i v :: *
  type IValM     i v = v

  type IConM     i v :: Constraint
  type IConM     i v =  NFData v

  -- | General lookup function.
  searchM      :: IConM i v => TextSearchOp -> IKeyM i v -> i v -> m [(IKeyM i v, IValM i v)]

  -- | Search within a range.
  lookupRangeM :: IConM i v => IKeyM i v -> IKeyM i v -> i v -> m [(IKeyM i v, IValM i v)]

  -- | Insert occurrences.
  insertListM  :: IConM i v => [(IKeyM i v, IValM i v)] -> i v -> m (i v)

  insertM       :: (IConM i v) => IKeyM i v -> IValM i v -> i v -> m (i v)
  insertM k v i = insertListM [(k,v)] i

  -- | Delete as batch job
  deleteDocsM  :: IConM i v => DocIdSet -> i v -> m (i v)

  -- | Delete occurrences.
  deleteM      :: IConM i v => DocId -> i v -> m (i v)
  deleteM k i  = deleteDocsM (IS.singleton k) i

  -- | Empty Index
  emptyM       :: (IConM i v) => m (i v)

  -- | Convert an Index to a list. Can be used for easy conversion between different index
  -- implementations
  toListM      :: IConM i v => i v -> m [(IKeyM i v, IValM i v)]

  -- | Make index from list
  fromListM    :: IConM i v => [(IKeyM i v, IValM i v)] -> m (i v)

  -- | Support for index value transformations
  unionWithM   :: IConM i v
               => (IValM i v -> IValM i v -> IValM i v)
               -> i v -> i v -> m (i v)

  unionWithConvM :: (IConM i v, Monad m, IConM i v2)
                 => (v2 -> v) -> (v -> v2 -> v)
                 -> i v -> i v2 -> m (i v)

  -- TODO: non-rigid map
  mapM         :: IConM i v
               => (IValM i v -> IValM i v)
               -> i v -> m (i v)
  mapM f = mapMaybeM (Just . f)

  -- TODO: non-rigid map
  mapMaybeM    :: IConM i v
               => (IValM i v -> Maybe (IValM i v))
               -> i v -> m (i v)

  -- XXX: maybe less generic with just list?
  keysM        :: IConM i v
               => i v -> m [IKeyM i v]

-- ----------------------------------------------------------------------------

class Index i where
  type IKey      i v :: *

  type IVal      i v :: *
  type IVal      i v = v

  type ICon      i v :: Constraint
  type ICon      i v =  NFData v

  -- | General lookup function.
  search       :: ICon i v => TextSearchOp -> IKey i v -> i v -> [(IKey i v, IVal i v)]

  -- | Search within a range.
  lookupRange  :: ICon i v => IKey i v -> IKey i v -> i v -> [(IKey i v, IVal i v)]

  -- | Insert occurrences.
  insertList  :: ICon i v => [(IKey i v, IVal i v)] -> i v -> i v

  -- | Insert occurrences.
  insert       :: ICon i v => IKey i v -> IVal i v -> i v -> i v
  insert k v   = insertList [(k,v)]

  -- | Delete as batch job
  deleteDocs  :: ICon i v => DocIdSet -> i v -> i v

  -- | Delete occurrences.
  delete       :: ICon i v => DocId -> i v -> i v
  delete       = deleteDocs . IS.singleton

  -- | Empty Index
  empty        :: ICon i v => i v

  -- | Convert an Index to a list. Can be used for easy conversion between different index
  -- implementations
  toList       :: ICon i v => i v -> [(IKey i v, IVal i v)]

  -- | Make index from list
  fromList     :: ICon i v => [(IKey i v, IVal i v)] -> i v

  -- | Support for index value transformations
  unionWith    :: ICon i v
               => (IVal i v -> IVal i v -> IVal i v)
               -> i v -> i v -> i v

  unionWithConv:: (ICon i v, ICon i v2)
               => (v2 -> v) -> (v -> v2 -> v)
               -> i v -> i v2 -> i v

  -- TODO: non-rigid map
  map          :: ICon i v
               => (IVal i v -> IVal i v)
               -> i v -> i v
  map f = mapMaybe (Just . f)

  mapMaybe     :: ICon i v
               => (IVal i v -> Maybe (IVal i v))
               -> i v -> i v

  -- XXX: maybe less generic with just list?
  keys         :: ICon i v
               => i v -> [IKey i v]

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
