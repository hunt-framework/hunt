{-# LANGUAGE Rank2Types      #-}
module Hunt.ContextIndex.Segment where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema
import qualified Hunt.Scoring.SearchResult as SearchResult

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import           System.FilePath
import           System.IO
import qualified Text.Printf as Printf

modSegState :: SegmentState -> SegmentState -> SegmentState
modSegState SegUncommited SegDirty = SegDirtyAndUncommited
modSegState SegDirty SegUncommited = SegDirtyAndUncommited
modSegState _        s             = s

-- | Marks given documents as deleted. Segment is marked as dirty.
--
segmentDeleteDocs :: DocIdSet -> Segment dt -> Segment dt
segmentDeleteDocs dIds seg
  = seg { segState       = segState seg `modSegState` SegDirty
        , segDeletedDocs = dIds `mappend` segDeletedDocs seg
        }

-- | Marks given Context as deleted. Segment is marked dirty.
--
segmentDeleteContext :: Context -> Segment dt -> Segment dt
segmentDeleteContext cx seg
  = seg { segState      = segState seg `modSegState` SegDirty
        , segDeletedCxs = Set.insert cx (segDeletedCxs seg)
        }

-- | Returns the segments `DocTable`. Respects deleted documents.
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

-- | Searches a segment given a search function. Respects deleted contexts
--   and documents.
--
searchSegment :: (Monad m, Ix.HasSearchResult r) => Context
                 -> (forall i. (Ix.IndexImplCon i) => i -> m [r])
                 -> Segment dt -> m [r]
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
mergeSegments :: (MonadIO m, DocTable dt) => Schema -> Segment dt -> Segment dt -> m (Segment dt)
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
                    in Just $ merge (ctMerge (cxType st)) [ix1', ix2']
                  (Nothing, Nothing)   -> Nothing
            return (cx, newIx')

       let ctxMap
             = mkContextMap (Map.fromList
                             (List.map (second fromJust)
                              (List.filter (isJust . snd) newCxMap)))

       return Segment { segId          = max (segId seg1) (segId seg2)
                      , segIndex       = ctxMap
                      , segDocs        = newDt
                      , segState       = SegUncommited
                      , segDeletedDocs = mempty
                      , segDeletedCxs  = mempty
                      }
  where
    -- | This should be removed, modification of indicies is NOT ALLOWED..
    prepIx :: DocIdSet -> Ix.IndexImpl -> Ix.IndexImpl
    prepIx delDocs (Ix.IndexImpl ix)
      = Ix.mkIndex (Ix.deleteDocs delDocs ix `asTypeOf` ix)
