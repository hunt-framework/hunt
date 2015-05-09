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

segmentDeleteDocs :: DocIdSet -> Segment dt -> Segment dt
segmentDeleteDocs dIds seg
  = seg { segIsDirty     = True
        , segDeletedDocs = dIds `mappend` segDeletedDocs seg
        }

segmentDeleteContext :: Context -> Segment dt -> Segment dt
segmentDeleteContext cx seg
  = seg { segIsDirty    = True
        , segDeletedCxs = Set.insert cx (segDeletedCxs seg)
        }

segmentDocs :: (Monad m, DocTable dt) => Segment dt -> m dt
segmentDocs seg
  = DocTable.difference (segDeletedDocs seg) (segDocs seg)

searchSegment :: (Monad m, Ix.HasSearchResult r) => Context -> Segment dt
                 -> (forall i. (Ix.IndexImplCon i) => i -> m [r]) -> m [r]
searchSegment cx seg search
  = if Set.notMember cx (segDeletedCxs seg)
       then case Map.lookup cx (cxMap (segIndex seg)) of
             Just (Ix.IndexImpl ix)
               -> do rx <- search ix
                     return (fmap (Ix.mapSR delDocs) rx)
             Nothing -> return []
    else return []
  where
    delDocs
      = SearchResult.srDiffDocs (segDeletedDocs seg)

mergeSegments :: (Monad m, DocTable dt) => Segment dt -> Segment dt -> m (Segment dt)
mergeSegments seg1 seg2
  = undefined

commitDirtySegment :: (MonadIO m) => FilePath -> Segment dt -> m ()
commitDirtySegment dir seg
  = undefined

commitSegment :: (MonadIO m, Binary dt, DocTable dt)
                 => FilePath
                 -> Segment dt
                 -> m ()
commitSegment dir seg
  = liftIO $ do withFile (dir </> ixName) WriteMode $ \h ->
                  mapM_ (uncurry (commitIx h)) contexts
                withFile (dir </> docsName) WriteMode $ \h ->
                  do dt <- DocTable.difference (segDeletedDocs seg) (segDocs seg)
                     LByteString.hPut h (Put.runPut (put dt))
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
           liftIO (LByteString.hPut h (mkBytes rx))
        where
          mkBytes
            = Put.runPut . void. foldM writeTermDelta mempty

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
