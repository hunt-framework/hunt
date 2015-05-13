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
import           Hunt.Scoring.SearchResult (SearchResult)
import qualified Hunt.Scoring.SearchResult as SearchResult

import           Control.Applicative
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
import           Data.Text (Text)
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

-- | Merges two `Segment`s.
--
mergeSegments :: (Monad m, DocTable dt) => Schema -> Segment dt -> Segment dt -> m (Segment dt)
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


-- | Flushes deleted documents and deleted contexts to index directory
--
commitDirtySegment :: (MonadIO m) => FilePath -> Segment dt -> m ()
commitDirtySegment dir seg
  = liftIO $ do withFile (dir </> delDocsName) WriteMode $ \h ->
                  do LByteString.hPut h (Put.runPut (put (segDeletedDocs seg)))
                     hFlush h
                withFile (dir </> delCxName) WriteMode $ \h ->
                  do LByteString.hPut h (Put.runPut (put (segDeletedCxs seg)))
                     hFlush h
  where
    delDocsName
      = Printf.printf "%.10o.docs.del" (unSegmentId (segId seg))
    delCxName
      = Printf.printf "%.10o.cx.del" (unSegmentId (segId seg))

-- | Writes the immutable DocumentTable and ContextMap to index directory
--
commitSegment :: (MonadIO m, Binary dt, DocTable dt)
                 => FilePath
                 -> Segment dt
                 -> m ()
commitSegment dir seg
  = liftIO $ do withFile (dir </> ixName) WriteMode $ \h ->
                  do mapM_ (uncurry (commitIx h)) contexts
                     hFlush h

                withFile (dir </> docsName) WriteMode $ \h ->
                  do dt <- DocTable.difference (segDeletedDocs seg) (segDocs seg)
                     LByteString.hPut h (Put.runPut (put dt))
                     hFlush h
  where
    ixName
      = Printf.printf "%.10o.terms" (unSegmentId (segId seg))
    docsName
      = Printf.printf "%.10o.docs" (unSegmentId (segId seg))

    contexts
      = List.filter (\cx -> Set.notMember (fst cx) (segDeletedCxs seg))
        . Map.toAscList
        . cxMap
        . segIndex
        $ seg

    commitIx :: MonadIO m => Handle -> Context -> Ix.IndexImpl -> m ()
    commitIx h cx (Ix.IndexImpl ix)
      = do rx <- Ix.toListM ix
           liftIO $ LByteString.hPut h (mkBytes rx)
        where
          mkBytes
            = Put.runPut . void . foldM writeTermDelta mempty

          writeTermDelta lastTerm (term, sr)
            = do put (Text.length prefix)
                 put suffix
                 put (SearchResult.searchResultToOccurrences sr)
                 return term
            where
              (prefix, suffix)
                = case Text.commonPrefixes lastTerm term of
                   Just (p, _, s) -> (p, s)
                   _              -> (mempty, term)
