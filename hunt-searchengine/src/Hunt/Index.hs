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

import           Control.Arrow (second)
import           Control.DeepSeq
import           Data.Binary (Binary)
import qualified Data.List as L
import           GHC.Exts (Constraint)
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId


import qualified Hunt.Common.DocIdMap      as DM
import           Hunt.Common.DocIdSet      (DocIdSet)
import qualified Hunt.Common.DocIdSet      as DS
import           Hunt.Common.Occurrences   (Occurrences)
import qualified Hunt.Common.Occurrences   as Occ
import           Hunt.Scoring.Keys         (addDefScore)
import           Hunt.Scoring.Score
import           Hunt.Scoring.SearchResult
import           Prelude hiding (map)

-- ------------------------------------------------------------

-- | The interface, that an data type must support to be used
-- value in an index.
--
-- The Monoid instance gives us the mergeValues op.

class (Monoid v, Binary v, NFData v) => IndexValue v where
  fromOccurrences  :: Occurrences -> v
  toSearchResult   :: v -> SearchResult
  diffValues       :: DocIdSet -> v -> Maybe v


-- | Helper for converting lists of index values to search results

toSearchResults :: IndexValue u => [(x, u)] -> [(x, SearchResult)]
toSearchResults = L.map (second toSearchResult)

-- | Helper for converting lists of occurrences to search index values

fromOccurrenceList :: IndexValue u => [(x, Occurrences)] -> [(x, u)]
fromOccurrenceList = L.map (second fromOccurrences)

-- instances for IndexValue:
--
-- Occurrences are the most general type for index values,
-- the type containing the most information
--
-- DocIdSets are the least general type for index values,
-- the type containing the least information giving still correct answers,
-- but without any information about the relevance (score) of a document

instance IndexValue Occurrences where
  fromOccurrences    = id
  toSearchResult     = mkSRfromOccurrences
  diffValues s m     = let z = Occ.diffWithSet m s in
                       if Occ.null z then Nothing else Just z

{-instance IndexValue DenseOccurrences where
  fromOccurrences    = DM.pack
  toSearchResult     = mkSRfromOccurrences
  diffValues s m     = let z = DMP.diffWithSet m s in
                       if DMP.null z then Nothing else Just z
-}

instance IndexValue (DM.DocIdMap Score) where
  fromOccurrences    = occurrencesToDocIdMapScore
  toSearchResult     = mkSRfromScoredDocs
  diffValues s m     = let r = DM.diffWithSet m s in
                       if DM.null r then Nothing else Just r

instance IndexValue DocIdSet where
  fromOccurrences    = DS.fromList . DM.keys
  toSearchResult     = mkSRfromUnScoredDocs
  diffValues s1 s2   = let r = DS.difference s2 s1 in
                       if DS.null r then Nothing else Just r

-- ------------------------------------------------------------

-- | The index type class which needs to be implemented to be used by the 'Interpreter'.
--   The type parameter @i@ is the implementation.
--   The implementation must have a value type parameter.
class (IndexValue (IVal i)) => Index i where
  -- | The key type of the index.
  type IKey i :: *
  type IVal i :: *

  type ICon i :: Constraint
  type ICon i = ()

  -- | General lookup function.
  search        :: ICon i => TextSearchOp -> IKey i -> i -> [(IKey i, SearchResult)]

  -- | Search with a scoring of the result by comparing the search key with the key in the result
  -- and estimating the similarity of these keys.
  --
  -- The default implementation is attaching always the default score (1.0)
  searchSc      :: ICon i => TextSearchOp -> IKey i -> i -> [(IKey i, (Score, SearchResult))]
  searchSc op k ix = addDefScore $ search op k ix

  -- | Search within a range of two keys.
  lookupRange   :: ICon i => IKey i -> IKey i -> i -> [(IKey i, SearchResult)]

  -- | Search withinin a range and scoring of the result by
  -- comparing the keys of the bounds with the key in the result
  -- and estimating the similarity of these keys.
  --
  -- The default implementation is attaching always the default score (1.0)
  lookupRangeSc :: ICon i => IKey i -> IKey i -> i -> [(IKey i, (Score, SearchResult))]
  lookupRangeSc k1 k2 ix
                = addDefScore $ lookupRange k1 k2 ix

  -- | Insert occurrences.
  --   This is more efficient than folding with 'insert'.
  insertList    :: ICon i =>
                   [(IKey i, Occurrences)] -> i -> i

  -- | Insert occurrences.
  insert        :: ICon i =>
                   IKey i -> Occurrences -> i -> i
  insert   k v  = insertList [(k,v)]

  -- | Delete as batch job.
  --   This is more efficient than folding with 'delete'.
  deleteDocs    :: ICon i => DocIdSet -> i -> i

  -- | Delete occurrences.
  delete        :: ICon i => DocId -> i -> i
  delete        = deleteDocs . DS.singleton

  -- | Empty index.
  empty         :: ICon i => i

  -- | Convert an index to a list.
  --   Can be used for easy conversion between different index implementations.
  toList        :: ICon i => i -> [(IKey i, SearchResult)]

  -- | Convert a list of key-value pairs to an index.
  fromList      :: ICon i => [(IKey i, Occurrences)] -> i

  -- | Merge two indexes with a combining function.
  unionWith     :: ICon i
                => (IVal i -> IVal i -> IVal i)
                -> i -> i -> i

  --   Merge two indexes with combining functions.
  --   The second index may have another value type than the first one.
  --   Conversion and merging of the indexes is done in a single step.
  --   This is much more efficient than mapping the second index and calling 'unionWith'.
  --  unionWithConv :: (ICon i, ICon i2)
  --                => IVal i)2 -> IVal i) -> (v -> v2 -> IVal i)
  --                -> i -> i2 -> i

  -- TODO: non-rigid map
  -- | Map a function over the values of the index.
  map           :: ICon i
                => (IVal i -> IVal i)
                -> i -> i
  map f = mapMaybe (Just . f)

  -- | Updates a value or deletes it if the result of the function is 'Nothing'.
  mapMaybe      :: ICon i
                => (IVal i -> Maybe (IVal i))
                -> i -> i

  -- | Keys of the index.
  keys          :: ICon i
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
  searchM      :: IConM i => TextSearchOp -> IKeyM i -> i -> m [(IKeyM i, SearchResult)]

  -- | Monadic version of 'search' with (default) scoring.
  searchMSc     :: IConM i => TextSearchOp -> IKeyM i -> i -> m [(IKeyM i, (Score, SearchResult))]
  searchMSc op k ix
                = searchM op k ix >>= return . addDefScore

  -- | Monadic version of 'lookupRangeM'.
  lookupRangeM :: IConM i => IKeyM i -> IKeyM i -> i -> m [(IKeyM i, SearchResult)]

  lookupRangeMSc :: IConM i => IKeyM i -> IKeyM i -> i -> m [(IKeyM i, (Score, SearchResult))]
  lookupRangeMSc k1 k2 ix
                = lookupRangeM k1 k2 ix >>= return . addDefScore

  -- | Monadic version of 'insertList'.
  insertListM  :: IConM i =>
                  [(IKeyM i, Occurrences)] -> i -> m (i)

  -- | Monadic version of 'insert'.
  insertM      :: IConM i =>
                  IKeyM i -> Occurrences -> i -> m (i)
  insertM k v  = insertListM [(k,v)]

  -- | Monadic version of 'deleteDocs'.
  deleteDocsM  :: IConM i => DocIdSet -> i -> m (i)

  -- | Monadic version of 'delete'.
  deleteM      :: IConM i => DocId -> i -> m (i)
  deleteM k i  = deleteDocsM (DS.singleton k) i

  -- | Monadic version of 'empty'.
  emptyM       :: IConM i => m (i)

  -- | Monadic version of 'toList'.
  toListM      :: IConM i => i -> m [(IKeyM i, SearchResult)]

  -- | Monadic version of 'fromList'.
  fromListM    :: IConM i => [(IKeyM i, Occurrences)] -> m (i)

  -- | Monadic version of 'unionWith'.
  unionWithM   :: IConM i =>
                  (IValM i -> IValM i -> IValM i) ->
                  i -> i -> m (i)

  --  Monadic version of 'unionWithConv'.
  -- unionWithConvM :: (IConM i, Monad m, IConM i2)
  --               => IVal i)2 -> IVal i) -> (v -> v2 -> IVal i)
  --               -> i -> i2 -> m (i)

  -- | Monadic version of 'map'.
  mapM         :: IConM i
               => (IValM i -> IValM i)
               -> i -> m (i)
  mapM f = mapMaybeM (Just . f)

  -- | Monadic version of 'mapMaybe'.
  mapMaybeM    :: IConM i
               => (IValM i -> Maybe (IValM i))
               -> i -> m (i)

  -- | Monadic version of 'keys'.
  keysM        :: IConM i
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
