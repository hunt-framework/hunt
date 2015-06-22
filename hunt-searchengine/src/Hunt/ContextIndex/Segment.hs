{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Segment where

import           Prelude hiding (mapM)

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema
import qualified Hunt.Scoring.SearchResult as SearchResult

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel as Par
import           Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LByteString
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Traversable
import           System.FilePath
import           System.IO
import qualified Text.Printf as Printf

newtype SegmentId
  = SegmentId { unSegmentId :: Int }
    deriving (Enum, Eq, Ord)

instance Show SegmentId where
  show (SegmentId i) = show i

newtype ContextMap
  = ContextMap { cxMap :: Map Context Ix.IndexImpl }
  deriving (Show)

data Segment dt
  = Segment { segIndex       :: !ContextMap
            , segDocs        :: !dt
            , segDeletedDocs :: !DocIdSet
            , segDeletedCxs  :: !(Set Context)
            }

newtype SegmentMap a
  = SegmentMap { unSegmentMap :: IntMap a }
  deriving (Functor)

data SegmentDiff
  = SegmentDiff !DocIdSet !(Set Context)

instance Monoid SegmentDiff where
  mempty
    = SegmentDiff mempty mempty
  mappend (SegmentDiff dids1 ctx1) (SegmentDiff dids2 ctx2)
    = SegmentDiff (dids1 <> dids2) (ctx1 <> ctx2)

mkContextMap :: Map Context Ix.IndexImpl -> ContextMap
mkContextMap m = ContextMap $! m

newContextMap' :: Schema -> ContextMap
newContextMap' = ContextMap . Map.map (newIx . ctIxImpl . cxType)
  where
    newIx :: Ix.IndexImpl -> Ix.IndexImpl
    newIx (Ix.IndexImpl i) = Ix.mkIndex (Ix.empty `asTypeOf` i)

-- | Create a new `SegmentMap`.
--
newSegmentMap :: SegmentMap dt
newSegmentMap
  = SegmentMap IntMap.empty

-- | Size of `SegmentMap`
size :: SegmentMap a -> Int
size
  = IntMap.size . unSegmentMap

elems :: SegmentMap a -> [a]
elems
  =  IntMap.elems . unSegmentMap

keys :: SegmentMap a -> [SegmentId]
keys
  = fmap SegmentId .  IntMap.keys . unSegmentMap

-- | Insert a `Segment` into a `SegmentMap`.
--
insert :: SegmentId -> Segment dt -> SegmentMap (Segment dt) -> SegmentMap (Segment dt)
insert (SegmentId sid) segment
  = SegmentMap . IntMap.insert sid segment . unSegmentMap

-- | Remove a `Segment` from `SegmentMap`
--
delete :: SegmentId -> SegmentMap dt -> SegmentMap dt
delete (SegmentId sid)
  = SegmentMap . IntMap.delete sid . unSegmentMap

unionWith :: (a -> a -> a) ->  SegmentMap a -> SegmentMap a -> SegmentMap a
unionWith f (SegmentMap m1) (SegmentMap m2)
  = SegmentMap (IntMap.unionWith f m1 m2)

intersectionWith :: (a -> b -> c) -> SegmentMap a -> SegmentMap b -> SegmentMap c
intersectionWith f (SegmentMap m1) (SegmentMap m2)
  = SegmentMap (IntMap.intersectionWith f m1 m2)

-- | Difference between two `SegmentMap`s.
--
difference :: SegmentMap a -> SegmentMap b -> SegmentMap a
difference (SegmentMap m1) (SegmentMap m2)
  = SegmentMap (IntMap.difference m1 m2)

differenceWith :: (a -> b -> Maybe a) -> SegmentMap a -> SegmentMap b -> SegmentMap a
differenceWith f (SegmentMap m1) (SegmentMap m2)
  = SegmentMap (IntMap.differenceWith f m1 m2)

-- | Creates a `SegmentMap` from a list of `Segment`s.
--
fromList :: [(SegmentId, a)] -> SegmentMap a
fromList
  = SegmentMap . IntMap.fromList . fmap (first unSegmentId)

-- | Returns a `SegmentMap` as list.
--
toList :: SegmentMap a -> [(SegmentId, a)]
toList
  = fmap (first SegmentId) .  IntMap.toList . unSegmentMap

-- | Pure mapping of Segments.
--
mapSegments :: (a -> b) -> SegmentMap a -> SegmentMap b
mapSegments f
  = SegmentMap . fmap f . unSegmentMap

-- | Maps Segments with a given function in monadic context.
--
mapSegmentsM :: (Functor m, Monad m)
             => (a -> m b)
             -> SegmentMap a
             -> m (SegmentMap b)
mapSegmentsM f (SegmentMap m)
  = SegmentMap <$> mapM f m

-- | Maps Segments with a given function in parallel.
--
mapSegmentsPar :: Par.MonadParallel m
               => (a -> m b)
               -> SegmentMap a
               -> m (SegmentMap b)
mapSegmentsPar f (SegmentMap m)
  = do r <- Par.mapM (\(sid, s) -> do r <- f s
                                      return (sid, r)
                     ) (IntMap.toList m)
       return (SegmentMap (IntMap.fromList r))

-- | Marks given documents as deleted.
--
segmentDeleteDocs :: DocIdSet -> Segment dt -> Segment dt
segmentDeleteDocs dIds seg
  = seg { segDeletedDocs = dIds `mappend` segDeletedDocs seg
        }

-- | Marks given Context as deleted.
--
segmentDeleteContext :: Context -> Segment dt -> Segment dt
segmentDeleteContext cx seg
  = seg { segDeletedCxs = Set.insert cx (segDeletedCxs seg)
        }

-- | Returns the segments `DocTable`.
--
segmentDocs :: (Monad m, DocTable dt) => Segment dt -> m dt
segmentDocs seg
  = DocTable.difference (segDeletedDocs seg) (segDocs seg)

-- | Returns the number of documents in this segment
--
segmentSize :: (Monad m, DocTable dt) => Segment dt -> m Int
segmentSize
  = DocTable.size . segDocs

-- | Returns the ratio between deleted docs and contained docs
--
segmentDeletedDocsRatio :: (Monad m, DocTable dt) => Segment dt -> m Float
segmentDeletedDocsRatio seg
  = do size <- segmentSize seg
       return (fromIntegral (DocIdSet.size (segDeletedDocs seg)) / fromIntegral size)

-- | Returns the `ContextMap` of a `Segment`. Respects deleted contexts.
--
segmentCxMap :: Monad m => Segment dt -> m ContextMap
segmentCxMap seg
  = return
    . mkContextMap
    . Map.filterWithKey (\k _ -> Set.notMember k (segDeletedCxs seg))
    . cxMap
    $ segIndex seg

-- | Since `Segment`s grow monotonically, e.g. only elements are
--   added never removed from their respective deleted docs/contexts sets
--   we can efficiently diff two `Segment`s.
--
segmentDiff :: Segment dt -> Segment dt -> SegmentDiff
segmentDiff s1 s2
  = if (DocIdSet.size (segDeletedDocs s1) < DocIdSet.size (segDeletedDocs s2))
       || (Set.size (segDeletedCxs s1) < Set.size (segDeletedCxs s2))
    then segmentDiff s2 s1
    else
      SegmentDiff
      (DocIdSet.difference (segDeletedDocs s1) (segDeletedDocs s2))
      (Set.difference (segDeletedCxs s1) (segDeletedCxs s2))

-- | Searches a segment given a search function. Respects deleted contexts
--   and documents.
--
searchSegment :: (Monad m, Ix.HasSearchResult r)
              => Context
              -> (forall i. (Ix.IndexImplCon i) => i -> m [r])
              -> Segment dt
              -> m [r]
searchSegment cx search seg
  = if Set.notMember cx (segDeletedCxs seg)
       then case Map.lookup cx (cxMap (segIndex seg)) of
             Just (Ix.IndexImpl ix)
               -> do rx <- search ix
                     return (if DocIdSet.null (segDeletedDocs seg)
                             then rx
                             else List.filter testNotEmpty
                                  . fmap (Ix.mapSR delDocs)
                                  $ rx
                            )
             Nothing -> return []
    else return []
  where
    delDocs
      = SearchResult.srDiffDocs (segDeletedDocs seg)

    testNotEmpty :: (Ix.HasSearchResult r) => r -> Bool
    testNotEmpty
      = not . Ix.testSR SearchResult.srNull
{-# INLINE searchSegment #-}

-- | Merges two `Segment`s.
--
mergeSegments :: (MonadIO m, DocTable dt)
              => Schema
              -> Segment dt
              -> Segment dt
              -> m (Segment dt)
mergeSegments schema seg1 seg2
  = do dt1 <- segmentDocs seg1
       dt2 <- segmentDocs seg2
       newDt <- DocTable.union dt1 dt2

       ContextMap m1 <- segmentCxMap seg1
       ContextMap m2 <- segmentCxMap seg2

       newCxMap <- forM (Map.toList schema) $ \(cx, st) ->
         do let cx1 = Map.lookup cx m1
                cx2 = Map.lookup cx m2
                newIx' = case (cx1, cx2) of
                  (Just ix1, Nothing)  -> Just ix1
                  (Nothing, Just ix2)  -> Just ix2
                  (Just ix1, Just ix2) ->
                    let ix1' = prepIx (segDeletedDocs seg1) ix1
                        ix2' = prepIx (segDeletedDocs seg2) ix2
                    in Just $ merge (ctMerge (cxType st)) ix1' ix2'
                  _                    -> Nothing
            return (cx, newIx')

       let ctxMap
             = mkContextMap (Map.fromList
                             (List.map (second fromJust)
                              (List.filter (isJust . snd) newCxMap)))

       return Segment { segIndex       = ctxMap
                      , segDocs        = newDt
                      , segDeletedDocs = mempty
                      , segDeletedCxs  = mempty
                      }
  where
    -- | This should be removed, modification of indicies is NOT ALLOWED..
    prepIx :: DocIdSet -> Ix.IndexImpl -> Ix.IndexImpl
    prepIx delDocs (Ix.IndexImpl ix)
      = Ix.mkIndex (Ix.deleteDocs delDocs ix `asTypeOf` ix)
