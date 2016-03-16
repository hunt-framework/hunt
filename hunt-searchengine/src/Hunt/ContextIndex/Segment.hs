{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
module Hunt.ContextIndex.Segment where

import           Prelude                   hiding (Word, mapM)

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap      (DocIdMap)
import           Hunt.Common.DocIdSet      (DocIdSet)
import qualified Hunt.Common.DocIdSet      as DocIdSet
import           Hunt.Common.Occurrences   (Occurrences)
import qualified Hunt.Common.Occurrences   as Occ
import           Hunt.Common.SegmentMap    (SegmentMap)
import qualified Hunt.Common.SegmentMap    as SegmentMap
import           Hunt.DocTable             (DocTable)
import qualified Hunt.DocTable             as DocTable
import qualified Hunt.Index                as Ix
import qualified Hunt.Index.IndexImpl      as Ix
import           Hunt.Index.Schema
import           Hunt.Scoring.SearchResult (SearchResult)
import qualified Hunt.Scoring.SearchResult as SearchResult
import           Hunt.Utility

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel    as Par
import           Data.Coerce
import qualified Data.List                 as List
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Unsafe.Coerce

newtype ContextMap
  = ContextMap { cxMap :: Map Context Ix.IndexImpl }
  deriving (Show, NFData)

-- |At any given time the ContextIndex has exactly one Active
-- Segment and zero or more Frozen Segments.
data Kind = Active
          | Frozen

data Segment (k :: Kind) dt
  = Segment { segIndex       :: !ContextMap
            , segNumDocs     :: !Int
            , segDocs        :: !dt
            , segDeletedDocs :: !DocIdSet
            , segDeletedCxs  :: !(Set Context)
            }

data SegmentDiff
  = SegmentDiff !DocIdSet !(Set Context)

instance Monoid SegmentDiff where
  mempty
    = SegmentDiff mempty mempty
  mappend (SegmentDiff dids1 ctx1) (SegmentDiff dids2 ctx2)
    = SegmentDiff (dids1 <> dids2) (ctx1 <> ctx2)

instance NFData dt => NFData (Segment k dt) where
  rnf s = rnf (segIndex s)
          `seq` rnf (segNumDocs s)
          `seq` rnf (segDocs s)
          `seq` rnf (segDeletedDocs s)
          `seq` rnf (segDeletedCxs s)

freeze :: Segment k dt -> Segment Frozen dt
freeze = coerce

-- |Derives an empty Segment from a Schema. New Segments are always Active.
emptySegment :: (Monad m, DocTable dt) => Schema -> m (Segment Active dt)
emptySegment schema =
  return Segment { segIndex = newContextMap' schema
                 , segNumDocs = 0
                 , segDocs = DocTable.empty
                 , segDeletedDocs = mempty
                 , segDeletedCxs = mempty
                 }
  where
    newContextMap' :: Schema -> ContextMap
    newContextMap' = ContextMap . Map.map (newIx . ctIxImpl . cxType)
      where
        newIx :: Ix.IndexImpl -> Ix.IndexImpl
        newIx (Ix.IndexImpl i) = Ix.mkIndex (Ix.empty `asTypeOf` i)

-- | Marks given documents as deleted. Only Frozen Segments support marking.
deleteDocs :: DocIdSet -> Segment Frozen dt -> Segment Frozen dt
deleteDocs dIds seg
  = seg { segDeletedDocs = dIds `mappend` segDeletedDocs seg
        }

-- | This is unsafe since it mutates the segment.
-- Only permitted for the active segment!
activeDeleteDocs :: (Monad m, DocTable dt)
                    => DocIdSet -> Segment Active dt -> m (Segment Active dt)
activeDeleteDocs dIds seg = do
  newDt <- DocTable.difference dIds (segDocs seg)
  newSize <- DocTable.size newDt
  return seg { segDocs = newDt
             , segNumDocs = newSize
             }

-- |Modifies a Document with a given function. Only Active Segments
-- may modify their DocTable.
modifyDoc :: (Monad m, DocTable dt)
          => (DocTable.DValue dt -> m (DocTable.DValue dt))
          -> DocId
          -> Segment Active dt
          -> m (Segment Active dt)
modifyDoc f did s = do
  newDt <- DocTable.adjust f did (segDocs s)
  return s { segDocs = newDt
           }

-- |Inserts a Context into an active Segment.
insertContext :: Context -> Ix.IndexImpl -> Segment Active dt -> Segment Active dt
insertContext cx ix seg
  = seg { segIndex = index'
        }
  where
    ContextMap index = segIndex seg
    index' = ContextMap (Map.insertWith (const id) cx ix index)

activeDeleteContexts :: (Monad m, DocTable dt)
                        => Set Context -> Segment Active dt -> m (Segment Active dt)
activeDeleteContexts cxs seg =
  undefined

-- | Marks given Context as deleted. Only Frozen Segments support marking.
deleteContext :: Context -> Segment Frozen dt -> Segment Frozen dt
deleteContext cx seg = deleteContexts (Set.singleton cx) seg

deleteContexts :: Set Context -> Segment Frozen dt -> Segment Frozen dt
deleteContexts cxs seg
  = seg { segDeletedCxs = cxs `mappend` segDeletedCxs seg
        }

-- | Returns the segments `DocTable`. Respects deleted docs.
--
segmentDocs :: (Monad m, DocTable dt) => Segment Frozen dt -> m dt
segmentDocs seg
  = DocTable.difference (segDeletedDocs seg) (segDocs seg)

activeSegmentDocs :: (Monad m, DocTable dt) => Segment Active dt -> m dt
activeSegmentDocs = return . segDocs

-- | Returns the number of documents in this segment
--
segmentSize :: (Monad m, DocTable dt) => Segment k dt -> m Int
segmentSize
  = return . segNumDocs

segmentSize' :: (Monad m, DocTable dt) => Segment Frozen dt -> m Int
segmentSize' seg
  = return (segNumDocs seg - DocIdSet.size (segDeletedDocs seg))

-- | Returns the ratio between deleted docs and contained docs
--
segmentDeletedDocsRatio :: (Monad m, DocTable dt) => Segment Frozen dt -> m Float
segmentDeletedDocsRatio seg
  = do size <- segmentSize seg
       return (fromIntegral (DocIdSet.size (segDeletedDocs seg)) / fromIntegral size)

-- | Returns the `ContextMap` of a `Segment`. Respects deleted contexts.
--
segmentCxMap :: Monad m => Segment Frozen dt -> m ContextMap
segmentCxMap seg
  = return
    . ContextMap
    . Map.filterWithKey (\k _ -> Set.notMember k (segDeletedCxs seg))
    . cxMap
    $ segIndex seg

activeSegmentCxMap :: Monad m => Segment Active dt -> m ContextMap
activeSegmentCxMap seg = return (segIndex seg)

-- | Since `Segment`s grow monotonically, e.g. only elements are
--   added never removed from their respective deleted docs/contexts sets
--   we can efficiently diff two `Segment`s.
--
diff :: Segment Frozen dt -> Segment Frozen dt -> SegmentDiff
diff s1 s2
  = if (DocIdSet.size (segDeletedDocs s1) < DocIdSet.size (segDeletedDocs s2))
       || (Set.size (segDeletedCxs s1) < Set.size (segDeletedCxs s2))
    then diff s2 s1
    else
      SegmentDiff
      (DocIdSet.difference (segDeletedDocs s1) (segDeletedDocs s2))
      (Set.difference (segDeletedCxs s1) (segDeletedCxs s2))

-- | Checks common `Segment`s for differences.
diff' :: SegmentMap (Segment Frozen dt) -> SegmentMap (Segment Frozen dt) -> SegmentDiff
diff' sm1 sm2
  = mconcat
    . SegmentMap.elems
    $ SegmentMap.intersectionWith diff sm1 sm2

-- | An interface for convenient post-search mapping on SearchResult
class HasSearchResult a where
  mapSR  :: (SearchResult -> SearchResult) -> a -> a
  testSR :: (SearchResult -> Bool) -> a -> Bool

instance HasSearchResult SearchResult where
  mapSR f = f
  {-# INLINE mapSR #-}

  testSR p = p
  {-# INLINE testSR #-}

instance HasSearchResult b => HasSearchResult (a, b) where
  mapSR f = second (mapSR f)
  {-# INLINE mapSR #-}

  testSR p = testSR p . snd
  {-# INLINE testSR #-}

-- | Searches a segment given a search function. Respects deleted contexts
--   and documents.
--
searchSegment :: (Monad m, HasSearchResult r)
              => Context
              -> (forall i. (Ix.IndexImplCon i) => i -> m [r])
              -> Segment k dt
              -> m [r]
searchSegment cx search seg
  = if Set.notMember cx (segDeletedCxs seg)
       then case Map.lookup cx (cxMap (segIndex seg)) of
             Just (Ix.IndexImpl ix)
               -> do rx <- search ix
                     return (if DocIdSet.null (segDeletedDocs seg)
                             then rx
                             else List.filter testNotEmpty
                                  . fmap (mapSR delDocs)
                                  $ rx
                            )
             Nothing -> return []
    else return []
  where
    delDocs
      = SearchResult.srDiffDocs (segDeletedDocs seg)

    testNotEmpty :: (HasSearchResult r) => r -> Bool
    testNotEmpty
      = not . testSR SearchResult.srNull
{-# INLINE searchSegment #-}

lookupDocument :: (Par.MonadParallel m, DocTable dt)
               => DocId
               -> Segment k dt
               -> m (Maybe (DocTable.DValue dt))
lookupDocument dId s
  = if not (isDeletedDoc dId s)
    then DocTable.lookup dId (segDocs s)
    else return Nothing

activeLookupDocument :: (Monad m, DocTable dt) =>
                        DocId -> Segment Active dt -> m (Maybe (DocTable.DValue dt))
activeLookupDocument dId seg = DocTable.lookup dId (segDocs seg)

lookupDocumentByURI :: (Monad m, DocTable dt)
                    => URI
                    -> Segment k dt
                    -> m (Maybe DocId)
lookupDocumentByURI uri s
  = do r <- DocTable.lookupByURI uri (segDocs s)
       case r of
         Nothing  -> return Nothing
         Just dId -> if not (isDeletedDoc dId s)
                     then return (Just dId)
                     else return Nothing

activeLookupDocumentByURI :: (Monad m, DocTable dt)
                          => URI -> Segment Active dt
                          -> m (Maybe DocId)
activeLookupDocumentByURI uri s =
  DocTable.lookupByURI uri (segDocs s)

-- | Returns wanted docs as `DocIdMap` to not expose DocTable.
selectDocuments :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => DocIdSet
                -> Segment k dt
                -> m (DocIdMap (DocTable.DValue dt))
selectDocuments dIds s
  = do newDt <- DocTable.restrict dIds' (segDocs s)
       DocTable.toMap newDt
  where
    dIds' = DocIdSet.difference dIds (segDeletedDocs s)

isDeletedDoc :: DocId -> Segment k dt -> Bool
isDeletedDoc dId
  = DocIdSet.member dId . segDeletedDocs

deleteDocsByURI :: (Functor m, Monad m, DocTable dt)
                => Set URI
                -> Segment Frozen  dt
                -> m (Segment Frozen dt)
deleteDocsByURI uris s
  = do dx <- mapM (\uri ->
                     fmap (fmap DocIdSet.singleton) (lookupDocumentByURI uri s)
                  ) (Set.toList uris)
       return $ maybe s (`deleteDocs` s) (mconcat dx)

activeDeleteDocsByURI :: (Monad m, DocTable dt)
                         => Set URI
                         -> Segment Active dt
                         -> m (Segment Active dt)
activeDeleteDocsByURI uris s = do
  newDt <- DocTable.differenceByURI uris (segDocs s)
  newNumDocs <- DocTable.size newDt
  return s { segDocs = newDt
           , segNumDocs = newNumDocs
           }

-- |Insert multiple documents and words into an already existing segment.
insertDocsAndWords :: (Par.MonadParallel m, Applicative m, DocTable dt)
                   => Schema
                   -> [(DocTable.DValue dt, Words)]
                   -> Segment Active dt
                   -> m (Segment Active dt)
insertDocsAndWords _schema docsAndWords seg = do
   -- insert to doctable and generate docId
  tablesAndWords <- Par.mapM createDocTableFromPartition $
    partitionListByLength 20 docsAndWords

  -- union doctables and docid-words pairs
  (numDocs, newDt, docIdsAndWords) <-
    unionDocTables tablesAndWords (segNumDocs seg) (segDocs seg)

  -- insert words to index
  newIx <- batchAddWordsM docIdsAndWords (segIndex seg)

  return seg { segIndex = newIx
             , segNumDocs = numDocs
             , segDocs = newDt
             }
  where
    -- takes list of documents with wordlist. creates new 'DocTable' and
    -- inserts each document of the list into it.
    createDocTableFromPartition :: (Par.MonadParallel m, DocTable dt)
                                  => [(DocTable.DValue dt, Words)]
                                  -> m (Int, dt, [(DocId, Words)])
    createDocTableFromPartition
      = foldM toDocTable (0, DocTable.empty, [])
      where
        toDocTable (numDocs, dt, resIdsAndWords) (doc, ws)
          = do (dId, dt') <- DocTable.insert doc dt
               return (numDocs + 1, dt', (dId, ws):resIdsAndWords)

    -- takes list of doctables with lists of docid-words pairs attached
    -- unions the doctables to one big doctable and concats the docid-words
    -- pairs to one list
    unionDocTables :: (Par.MonadParallel m, DocTable dt)
                   => [(Int, dt, [(DocId, Words)])]
                   -> Int
                   -> dt
                   -> m (Int, dt, [(DocId, Words)])
    unionDocTables tablesAndWords oldNumDocs oldDt
      = do step <- Par.mapM unionDtsAndWords $ mkPairs tablesAndWords
           case step of
             []      -> return (0, DocTable.empty, [])
             [(k, d,w)] -> do n <- DocTable.union oldDt d
                              return (k + oldNumDocs, n, w)
             xs      -> unionDocTables xs oldNumDocs oldDt
      where
        unionDtsAndWords ((n1, dt1, ws1), (n2, dt2, ws2))
          = do dt <- DocTable.union dt1 dt2
               return (n1 + n2, dt, ws1 ++ ws2)

        mkPairs []       = []
        mkPairs [a]      = [(a,(0,DocTable.empty,[]))]
        mkPairs (a:b:xs) = (a,b):mkPairs xs

    -- | Computes the words and occurrences out of a list for one context
    contentForCx :: Context -> [(DocId, Words)] -> [(Word, Occurrences)]
    contentForCx cx
      = concatMap (invert . second (getWlForCx cx))
      where
        invert (did, wl)
          = map (second (Occ.singleton' did)) $ Map.toList wl
        getWlForCx
          = Map.findWithDefault Map.empty

    batchAddWordsM :: (Functor m, Par.MonadParallel m) =>
                      [(DocId, Words)] -> ContextMap -> m ContextMap
    batchAddWordsM [] ix
      = return ix
    batchAddWordsM vs (ContextMap m)
     = mkContextMap <$>
      mapWithKeyMP ( \cx impl -> foldinsertList cx impl ) m
     where
       foldinsertList :: (Functor m, Monad m) =>
                         Context -> Ix.IndexImpl -> m Ix.IndexImpl
       foldinsertList cx (Ix.IndexImpl impl)
           = Ix.mkIndex <$>
             Ix.insertListM (contentForCx cx vs) impl

    mapWithKeyMP f m =
     (Par.mapM (\(k, a) -> do b <- f k a
                              return (k, b)
               ) $ Map.toAscList m) >>= return . Map.fromDistinctAscList

-- |Creates a new Segment from docs and words.
fromDocsAndWords :: (Par.MonadParallel m, Applicative m, DocTable dt)
                 => Schema
                 -> [(DocTable.DValue dt, Words)]
                 -> m (Segment Active dt)
fromDocsAndWords schema docsAndWords = do
  seg <- emptySegment schema
  insertDocsAndWords schema docsAndWords seg

mkContextMap :: Map Context Ix.IndexImpl -> ContextMap
mkContextMap x = ContextMap $! x

-- | Merges two `Segment`s. Merging of two `Segment`s boils down to merging
-- their doctables and merging their corresponding `ContextMap`s.
mergeSegments :: (MonadIO m, DocTable dt)
              => Schema
              -> Segment Frozen dt
              -> Segment Frozen dt
              -> m (Segment Frozen dt)
mergeSegments schema seg1 seg2
  = do dt1 <- segmentDocs seg1
       dt2 <- segmentDocs seg2
       newDt <- DocTable.union dt1 dt2

       ContextMap m1 <- segmentCxMap seg1
       ContextMap m2 <- segmentCxMap seg2

       let newCxMap = mergeCxMap m1 (segDeletedDocs seg1)
                                 m2 (segDeletedDocs seg2)
                                 (Map.toList schema)

       return Segment { segIndex       = ContextMap (Map.fromDistinctAscList newCxMap)
                                         -- can use fromAscList here, because the ordering
                                         -- is determined by the ordering of schema.
                      , segNumDocs     = segNumDocs seg1 + segNumDocs seg2
                      , segDocs        = newDt
                      , segDeletedDocs = mempty
                      , segDeletedCxs  = mempty
                      }
  where
    mergeCxMap m1 d1 m2 d2 = concatMap f
      where
        f (cx, _st) =
          case (Map.lookup cx m1, Map.lookup cx m2) of
          -- Context is missing in second ContextMap, nothing to merge here
          (Just ix1, Nothing) -> [ (cx, ix1) ]
          -- Context is missing in first ContextMap, nothing to merge here
          (Nothing, Just ix2) -> [ (cx, ix2) ]
          -- Context is present in both ContextMaps, merge indices
          (Just ix1, Just ix2) -> [ (cx, mergeIx (d1, ix1) (d2, ix2)) ]
          _ -> []

    mergeIx :: (DocIdSet, Ix.IndexImpl) -> (DocIdSet, Ix.IndexImpl) -> Ix.IndexImpl
    mergeIx (dd1, Ix.IndexImpl ix1) (dd2, Ix.IndexImpl ix2)
      -- As the indices are existentially quantified we need to unsafeCoerce here.
      = Ix.mkIndex $ Ix.unionWith (<>) ix1' (unsafeCoerce ix2')
      where
        -- FIXME: this is not correct. It removes the occurences from the index
        -- but not the words itself which is bad but works for the time being
        ix1' = Ix.map (fromMaybe mempty . Ix.diffValues dd1) ix1
        ix2' = Ix.map (fromMaybe mempty . Ix.diffValues dd2) ix2
