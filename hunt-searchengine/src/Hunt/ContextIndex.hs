{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Hunt.ContextIndex (
    -- * Construction
    empty

    -- * Contexts and Schema
  , insertContext
  , deleteContext
  , contexts
  , contextsM
  , defaultContexts
  , hasContext
  , hasContextM
  , schema

    -- * Queries
  , lookupRangeCx
  , searchWithCx
  , searchWithCxSc
  , lookupRangeCxSc
  , lookupAllWithCx

    -- * Insert\/Delete Documents
  , insertList
                                 -- XXX: these functions should be internal
                                 -- we export them to be able to test them
                                 -- is there a bedder approach to achieve this?
--  , createDocTableFromPartition  -- only used in tests
--  , unionDocTables               -- only used in tests
  , modifyWithDescription
  , delete
  , deleteDocsByURI
  , member

    -- * Documents
  , lookupDocumentByURI
  , lookupDocument
  , selectDocuments

  , ContextIndex

    -- * Merge specific
  , MergeDescr
  , MergeLock
  , MergePolicy(..)
  , ApplyMerge(..)
  , runMerge
  , tryMerge

  , status
  ) where

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc as DocDesc
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap (DocIdMap)
import qualified Hunt.Common.DocIdMap as DocIdMap
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.Common.Document as Doc
import           Hunt.Common.SegmentMap (SegmentMap, SegmentId(..))
import qualified Hunt.Common.SegmentMap as SegmentMap
import           Hunt.ContextIndex.Merge (MergeDescr(..), MergeLock, MergePolicy(..))
import qualified Hunt.ContextIndex.Merge as Merge
import           Hunt.ContextIndex.Status
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema
import           Hunt.Scoring.Score
import           Hunt.Scoring.SearchResult
import           Hunt.Segment (Segment, SegmentDiff(..))
import qualified Hunt.Segment as Segment

import           Control.Applicative hiding (empty)
import           Control.Monad.IO.Class
import qualified Control.Monad.Parallel as Par
import           Data.Binary
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as Trav

data ContextIndex dt
  = ContextIndex { ciSegments      :: !(SegmentMap (Segment dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciMergeLock     :: !MergeLock
                 }

instance Binary (ContextIndex dt) where
  get = undefined
  put = undefined

newtype ApplyMerge dt
  = ApplyMerge { applyMerge  :: ContextIndex dt -> ContextIndex dt }

instance Monoid (ApplyMerge dt) where
  mempty
    = ApplyMerge id
  mappend (ApplyMerge f) (ApplyMerge g)
    = ApplyMerge (f . g)

empty :: DocTable dt => ContextIndex dt
empty
  = ContextIndex { ciSegments      = SegmentMap.empty
                 , ciSchema        = mempty
                 , ciNextSegmentId = SegmentId 1
                 , ciMergeLock     = mempty
                 }

-- | Inserts a new `Context` with `ContextSchema` into the `ContextIndex`.
insertContext :: Context
              -> Ix.IndexImpl
              -> ContextSchema
              -> ContextIndex dt
              -> ContextIndex dt
insertContext cx _ix s ixx
  = ixx { ciSchema = Map.insertWith (const id) cx s (ciSchema ixx) }

-- | Removes a `Context` from the index.
deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciSegments = fmap (Segment.deleteContext cx) (ciSegments ixx)
        , ciSchema   = Map.delete cx (ciSchema ixx)
        }

-- | Returns any `Context` which is searched by default.
defaultContexts :: ContextIndex dt -> [Context]
defaultContexts
  = Map.keys . Map.filter cxDefault . ciSchema

-- | Returns all contexts in the index.
contexts :: ContextIndex dt -> [Context]
contexts
  = Map.keys . ciSchema

-- | See `contexts`.
contextsM :: Monad m => ContextIndex dt -> m [Context]
contextsM
  = return . contexts

-- | Checks for `Context` existence.
hasContext :: Context -> ContextIndex dt -> Bool
hasContext cx
  = Map.member cx . ciSchema

-- | See `hasContext`.
hasContextM :: Monad m => Context -> ContextIndex dt -> m Bool
hasContextM cx
  = return . hasContext cx

-- | Returns the index `Schema`.
schema :: ContextIndex dt -> Schema
schema = ciSchema

--   This is more efficient than using fold and with 'insert'.
-- | Insert multiple documents and words.
insertList :: (Par.MonadParallel m, Applicative m, DocTable dt)
           => [(DocTable.DValue dt, Words)]
           -> ContextIndex dt
           -> m (ContextIndex dt)
insertList docAndWords ixx
  = do newSeg <- Segment.fromDocsAndWords (ciSchema ixx) docAndWords
       return $! ixx { ciSegments      =
                           SegmentMap.insert (ciNextSegmentId ixx) newSeg (ciSegments ixx)
                     , ciNextSegmentId = succ (ciNextSegmentId ixx)
                     }

-- | Modify the descirption of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Par.MonadParallel m, Applicative m, DocTable dt)
                      => Score
                      -> Description
                      -> Words
                      -> DocId
                      -> ContextIndex dt
                      -> m (ContextIndex dt)
modifyWithDescription weight descr wrds dId ixx
    = do Just doc <- lookupDocument dId ixx -- TODO: dangerous
         ixx'     <- delete' (DocIdSet.singleton dId) ixx
         insertList [(mergeDescr doc, wrds)] ixx'
  where
      -- M.union is left-biased
      -- flip to use new values for existing keys
      -- no flip to keep old values
      --
      -- Null values in new descr will remove associated attributes
    mergeDescr
      = wrap . Doc.update (updateWeight . updateDescr) . unwrap
      where
        updateWeight d
          | weight == noScore = d
          | otherwise         = d {wght = weight}

        updateDescr d           = -- trc "updateDescr res=" $
          d {desc = DocDesc.deleteNull $
                    flip DocDesc.union d' descr'
            }
          where
            d'     = -- trc "updateDescr old=" $
              desc d
            descr' = -- trc "updateDescr new=" $
              descr

-- | Search query in a context.
searchWithCx :: Par.MonadParallel m
             => TextSearchOp
             -> Context
             -> Text
             -> ContextIndex dt
             -> m [(Text, SearchResult)]
searchWithCx op cx w ix
  = lookupIndex cx ix merge (Ix.searchM op w)

searchWithCxSc :: Par.MonadParallel m
               => TextSearchOp
               -> Context
               -> Text
               -> ContextIndex dt
               -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ix
  = lookupIndex cx ix merge (Ix.searchMSc op w)

lookupRangeCx :: Par.MonadParallel m
              => Context
              -> Text
              -> Text
              -> ContextIndex dt
              -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeM k1 k2)

lookupRangeCxSc :: Par.MonadParallel m
                => Context
                -> Text
                -> Text
                -> ContextIndex dt
                -> m [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeMSc k1 k2)

lookupAllWithCx :: Par.MonadParallel m
                => Context
                -> ContextIndex dt
                -> m [(Text, SearchResult)]
lookupAllWithCx c ix
  = lookupIndex c ix merge Ix.toListM

-- | lookup an index by a context and then search this index for a word
--   result is always a list of values.
--
--   This pattern is used in all search variants
lookupIndex :: (Par.MonadParallel m, Ix.HasSearchResult r)
            => Context
            -> ContextIndex dt
            -> ([[r]] -> [r])
            -> (forall i . Ix.IndexImplCon i => i -> m [r])
            -> m [r]
lookupIndex cx ixx mrg search
  = do rx <- mapIxsP (Segment.searchSegment cx search) ixx
       return (mrg rx)
{-# INLINE lookupIndex #-}

-- | Merge function for `SearchResult`s.
merge :: (Ord a, Monoid b) => [[(a, b)]] -> [(a, b)]
merge
  = Map.toList . Map.unionsWith mappend . fmap Map.fromList
{-# INLINE merge #-}

lookupDocumentByURI :: (Par.MonadParallel m, DocTable dt)
                    => URI
                    -> ContextIndex dt
                    -> m (Maybe DocId)
lookupDocumentByURI uri ixx
  = do dx <- mapIxsP (Segment.lookupDocumentByURI uri) ixx
       return
         . listToMaybe
         . catMaybes $ dx

lookupDocument :: (Par.MonadParallel m, DocTable dt)
               => DocId
               -> ContextIndex dt
               -> m (Maybe (DocTable.DValue dt))
lookupDocument dId ixx
  = do dx <- mapIxsP (Segment.lookupDocument dId) ixx
       return
         . listToMaybe
         . catMaybes $ dx

selectDocuments :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => DocIdSet
                -> ContextIndex dt
                -> m (DocIdMap (DocTable.DValue dt))
selectDocuments dIds ixx
  = do dx <- mapIxsP (Segment.selectDocuments dIds) ixx
       return (DocIdMap.unionsWith undefined dx)

-- | Is the document part of the index?
member :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => URI
       -> ContextIndex dt
       -> m Bool
member u ixx = do
  mems <- mapIxsP (DocTable.lookupByURI u . Segment.segDocs) ixx
  return (List.any isJust mems)

-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, DocTable dt)
       => DocIdSet
       -> ContextIndex dt
       -> m (ContextIndex dt)
delete dIds ixx
    | DocIdSet.null dIds = return ixx
    | otherwise          = delete' dIds ixx

delete' :: (Par.MonadParallel m, DocTable dt)
        => DocIdSet
        -> ContextIndex dt
        -> m (ContextIndex dt)
delete' dIds ixx
  = do sm <- Trav.mapM (return . Segment.deleteDocs dIds) (ciSegments ixx)
       return ixx { ciSegments = sm }

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => Set URI
                -> ContextIndex dt
                -> m (ContextIndex dt)
deleteDocsByURI us ixx
  = do sx <- Trav.for (ciSegments ixx) (Segment.deleteDocsByURI us)
       return ixx { ciSegments = sx }

-- | Selects segments viable to merge but don't actually do the merge
--   because it can be a quite costly operation. Marks segments as
--   merging.
tryMerge :: (Functor m, Monad m, DocTable dt)
         => MergePolicy
         -> ContextIndex dt
         -> m ([MergeDescr dt], ContextIndex dt)
tryMerge policy ixx
  = do (merges, lock, nextSegmentId) <-
         Merge.selectMerges policy (ciMergeLock ixx) (ciSchema ixx) (ciNextSegmentId ixx) (ciSegments ixx)
       let ixx' = ixx { ciMergeLock     = lock
                      , ciNextSegmentId = nextSegmentId
                      }
       return (merges, ixx')

-- | Takes a list of merge descriptions and performs the merge of the
--   segments. Returns an idempotent function which can be applied to the
--   `ContextIndex`. This way, the costly merge can be done asynchronously.
runMerge :: (MonadIO m, DocTable dt) => MergeDescr dt -> m (ApplyMerge dt)
runMerge descr
  = do !newSeg <- Merge.runMerge descr
       return $
         ApplyMerge (applyMergedSegment (mdSegId descr) (mdSegs descr) newSeg)

-- | Since merging can happen asynchronously, we have to account for documents
--   and contexts deleted while we were merging the segments.
applyMergedSegment :: SegmentId
                   -> SegmentMap (Segment dt)
                   -> Segment dt
                   -> ContextIndex dt
                   -> ContextIndex dt
applyMergedSegment segmentId oldSegments newSegment ixx
  = ixx { ciSegments      =
              SegmentMap.insertWith (const id) segmentId newSegment' (
                SegmentMap.difference (ciSegments ixx) oldSegments)
        , ciNextSegmentId = succ (ciNextSegmentId ixx)
        , ciMergeLock     = Merge.releaseLock (ciMergeLock ixx) oldSegments
        }
  where
    newSegment'
      = Segment.deleteDocs deltaDelDocs
        . Segment.deleteContexts deltaDelCx
        $ newSegment

    SegmentDiff deltaDelDocs deltaDelCx
      = Segment.diff' oldSegments (ciSegments ixx)

mapIxsP :: Par.MonadParallel m => (Segment dt -> m a) -> ContextIndex dt -> m [a]
mapIxsP f
  = Par.mapM f . fmap snd . SegmentMap.toList  . ciSegments
{-# INLINE mapIxsP #-}
