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
import           Hunt.Scoring.SearchResult (SearchResult)
import qualified Hunt.Scoring.SearchResult as SearchResult

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.FilePath
import           System.IO
import qualified Text.Printf as Printf

-- | Marks given documents as deleted. Segment is marked as dirty.
--
segmentDeleteDocs :: DocIdSet -> Segment dt -> Segment dt
segmentDeleteDocs dIds seg
  = seg { segIsDirty     = True
        , segDeletedDocs = dIds `mappend` segDeletedDocs seg
        }

-- | Marks given Context as deleted. Segment is marked dirty.
--
segmentDeleteContext :: Context -> Segment dt -> Segment dt
segmentDeleteContext cx seg
  = seg { segIsDirty    = True
        , segDeletedCxs = Set.insert cx (segDeletedCxs seg)
        }

-- | Returns the segments `DocTable`. Respects deleted documents.
--
segmentDocs :: (Monad m, DocTable dt) => Segment dt -> m dt
segmentDocs seg
  = DocTable.difference (segDeletedDocs seg) (segDocs seg)

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
mergeSegments :: (Monad m, DocTable dt) => Segment dt -> Segment dt -> m (Segment dt)
mergeSegments seg1 seg2
  = undefined

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
