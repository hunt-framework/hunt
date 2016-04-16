{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- ----------------------------------------------------------------------------
{- |
  The context index introduces contexts and combines the index, document table and schema.
-}
-- ----------------------------------------------------------------------------

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
  , IndexAction
  , runIxAction

  , status
  ) where

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc       as DocDesc
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap      (DocIdMap)
import qualified Hunt.Common.DocIdMap      as DocIdMap
import           Hunt.Common.DocIdSet      (DocIdSet)
import qualified Hunt.Common.DocIdSet      as DocIdSet
import           Hunt.Common.Document      as Doc
import           Hunt.Common.SegmentMap    (SegmentId (..))
import qualified Hunt.Common.SegmentMap    as SegmentMap
import qualified Hunt.ContextIndex.Flush   as Flush
import qualified Hunt.ContextIndex.Lock    as Lock
import qualified Hunt.ContextIndex.Merge   as Merge
import           Hunt.ContextIndex.Segment (Docs, Kind (..), Segment)
import qualified Hunt.ContextIndex.Segment as Segment
import           Hunt.ContextIndex.Status
import           Hunt.ContextIndex.Types
import           Hunt.DocTable             (DValue, DocTable)
import qualified Hunt.DocTable             as DocTable
import qualified Hunt.Index                as Ix
import qualified Hunt.Index.IndexImpl      as Ix
import           Hunt.Index.Schema
import           Hunt.Scoring.Score
import           Hunt.Scoring.SearchResult

import qualified Control.Monad.Parallel    as Par
import           Data.Binary               (Binary)
import           Data.Coerce
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Maybe

import           Data.Set                  (Set)

import           Data.Text                 (Text)
import           Data.Traversable

type RunCIx m = ( Segment.RunSegment m
                , Functor m
                , Applicative m
                , Monad m
                )

empty :: (RunCIx m) => MergePolicy -> m ContextIndex
empty mergePolicy = do
  active <- Segment.emptySegment mempty
  return ContextIndex { ciActiveSegment = active
                      , ciSegments      = SegmentMap.empty
                      , ciSchema        = mempty
                      , ciNextSegmentId = SegmentId 1
                      , ciMergePolicy   = mergePolicy
                      , ciSegmentLock   = mempty
                      }

-- | Inserts a new `Context` with `ContextSchema` into the `ContextIndex`.
insertContext :: Context
              -> Ix.IndexImpl
              -> ContextSchema
              -> ContextIndex
              -> ContextIndex
insertContext cx ix s ixx
  = ixx { ciSchema = Map.insertWith (const id) cx s (ciSchema ixx)
        , ciActiveSegment = Segment.insertContext cx ix (ciActiveSegment ixx)
        }

-- | Removes a `Context` from the index.
deleteContext :: Context -> ContextIndex -> ContextIndex
deleteContext cx ixx
  = ixx { ciSegments = fmap (Segment.deleteContext cx) (ciSegments ixx)
        , ciSchema   = Map.delete cx (ciSchema ixx)
        , ciActiveSegment = Segment.activeDeleteContext cx (ciActiveSegment ixx)
        }

-- | Returns any `Context` which is searched by default.
defaultContexts :: ContextIndex -> [Context]
defaultContexts
  = Map.keys . Map.filter cxDefault . ciSchema

-- | Returns all contexts in the index.
contexts :: ContextIndex -> [Context]
contexts
  = Map.keys . ciSchema

-- | See `contexts`.
contextsM :: Monad m => ContextIndex -> m [Context]
contextsM
  = return . contexts

-- | Checks for `Context` existence.
hasContext :: Context -> ContextIndex -> Bool
hasContext cx
  = Map.member cx . ciSchema

-- | See `hasContext`.
hasContextM :: Monad m => Context -> ContextIndex -> m Bool
hasContextM cx
  = return . hasContext cx

-- | Returns the index `Schema`.
schema :: ContextIndex -> Schema
schema = ciSchema

--   This is more efficient than using fold and with 'insert'.
-- | Insert multiple documents and words.
insertList :: (RunCIx m, Par.MonadParallel m, Binary (DValue Docs))
           => [(DocTable.DValue Docs, Words)]
           -> ContextIndex
           -> m (ContextIndex, [IndexAction])
insertList docsAndWords ixx
 = do active' <- Segment.insertDocsAndWords (ciSchema ixx) docsAndWords (ciActiveSegment ixx)
      -- check if active Segment reached the size threshold.
      -- We use the same quantification as for merging Segments.
      level <- Merge.quantify' Segment.segmentSize (ciMergePolicy ixx) active'
      let threshold = mpMaxActiveSegmentLevel (ciMergePolicy ixx)
      if level >= threshold
        then do -- Freeze the active Segment and replace it with an empty one.
                newActive <- Segment.emptySegment (ciSchema ixx)
                insertSegment active' (ixx { ciActiveSegment = newActive })
        else do let ixx' = ixx { ciActiveSegment = active' }
                return (ixx', mempty)

-- | Inserts a segment into the index. Assigns a `SegmentId` to the `Segment`.
insertSegment :: (Monad m, Binary (DValue Docs)) => Segment 'Active
              -> ContextIndex -> m (ContextIndex , [IndexAction])
insertSegment seg ixx = do
  let
    sid    = ciNextSegmentId ixx
    frozen = Segment.freeze seg
    ixx'   = ixx { ciSegments =
                     SegmentMap.insert sid frozen (ciSegments ixx)
                 , ciNextSegmentId = succ sid
                 }

    -- Check if we can merge Segments with the new Segment inserted into the ContextIndex.
    -- A merge can trigger a cascade of merges until the index is stable and no merges
    -- are required anymore.
    mkMergeAct :: (Monad m, Binary (DValue Docs)) =>
                  ContextIndex -> m (ContextIndex, [IndexAction])
    mkMergeAct cix = do
      (mergeDescrs, cix') <- Merge.tryMerge cix

      let
        -- TODO: Make merging symmetric with flushing:
        -- Locking and unlocking of Segments currently happens in Merge module
        -- make it happen here like with flushing.
        _lock :: Lock.SegmentLock
        _lock = Merge.lockFromDescrs mergeDescrs

        action !descr = IndexAction $ do
            !modIx <- Merge.runMerge Segment.mergeSegments descr
            return $ \ix -> do
              let (newSid, newSeg, ix') = modIx ix
              -- Make sure to flush newly merged segments
              -- before cascading the merge
              -- TODO: maybe we can interleave merging and flushing
              mkFlushAct newSid newSeg ix'

      return (cix', fmap action mergeDescrs)

    -- A flush action flushes the just frozen segment to disk. It makes sure
    -- to lock the segment to prevent premature merges until its fully flushed.
    -- After flushing it triggers possible merges.
    mkFlushAct :: (Monad m, Binary (DValue Docs)) => SegmentId
               -> Segment 'Frozen -> ContextIndex -> m (ContextIndex, [IndexAction])
    mkFlushAct segId segment cix = do
      let cix' = cix { ciSegmentLock = Lock.lock segId (ciSegmentLock cix)
                     }
          act = IndexAction $ do
            !modIx <- Flush.runFlush (FlushPolicy "index") segId segment
            return $ \ix -> do
              mkMergeAct $ modIx ix { ciSegmentLock =
                                        Lock.release segId (ciSegmentLock ix)
                                    }
      return (cix', [act])

  mkFlushAct sid frozen ixx'

-- | Modify the descirption of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (RunCIx m, Par.MonadParallel m, Binary (DValue Docs))
                      => Score
                      -> Description
                      -> Words
                      -> DocId
                      -> ContextIndex
                      -> m (ContextIndex, [IndexAction])
modifyWithDescription weight descr wrds dId ixx
    = do let as = ciActiveSegment ixx
         -- The active segment is always in memory and mutable
         -- Doc deletion is cheap then!
         mdoc <- Segment.activeLookupDocument dId as
         case mdoc of
           Just _ -> do as' <- Segment.modifyDoc mergeDescr dId as
                        as'' <- Segment.batchAddWordsM [(dId, wrds)] as'
                        return (ixx { ciActiveSegment = as'' }, mempty)
           Nothing -> do Just doc <- lookupDocument dId ixx
                         ixx' <- delete' (DocIdSet.singleton dId) ixx
                         newDoc <- mergeDescr doc
                         insertList [(newDoc, wrds)] ixx'
  where
      -- M.union is left-biased
      -- flip to use new values for existing keys
      -- no flip to keep old values
      --
      -- Null values in new descr will remove associated attributes
    mergeDescr
      = return . wrap . Doc.update (updateWeight . updateDescr) . unwrap
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
             -> ContextIndex
             -> m [(Text, SearchResult)]
searchWithCx op cx w ix
  = lookupIndex cx ix merge (Ix.searchM op w)

searchWithCxSc :: Par.MonadParallel m
               => TextSearchOp
               -> Context
               -> Text
               -> ContextIndex
               -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ix
  = lookupIndex cx ix merge (Ix.searchMSc op w)

lookupRangeCx :: Par.MonadParallel m
              => Context
              -> Text
              -> Text
              -> ContextIndex
              -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeM k1 k2)

lookupRangeCxSc :: Par.MonadParallel m
                => Context
                -> Text
                -> Text
                -> ContextIndex
                -> m [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeMSc k1 k2)

lookupAllWithCx :: Par.MonadParallel m
                => Context
                -> ContextIndex
                -> m [(Text, SearchResult)]
lookupAllWithCx c ix
  = lookupIndex c ix merge Ix.toListM

-- | lookup an index by a context and then search this index for a word
--   result is always a list of values.
--
--   This pattern is used in all search variants
lookupIndex :: (Par.MonadParallel m, Segment.HasSearchResult r)
            => Context
            -> ContextIndex
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

lookupDocumentByURI :: (RunCIx m, Par.MonadParallel m)
                    => URI
                    -> ContextIndex
                    -> m (Maybe DocId)
lookupDocumentByURI docUri ixx
  = do dx <- mapIxsP (Segment.lookupDocumentByURI docUri) ixx
       return
         . listToMaybe
         . catMaybes $ dx

lookupDocument :: (RunCIx m, Par.MonadParallel m)
               => DocId
               -> ContextIndex
               -> m (Maybe (DocTable.DValue Docs))
lookupDocument dId ixx
  = do dx <- mapIxsP (Segment.lookupDocument dId) ixx
       return
         . listToMaybe
         . catMaybes $ dx

selectDocuments :: (RunCIx m, Par.MonadParallel m)
                => DocIdSet
                -> ContextIndex
                -> m (DocIdMap (DocTable.DValue Docs))
selectDocuments dIds ixx
  = do dx <- mapIxsP (Segment.selectDocuments dIds) ixx
       return (DocIdMap.unionsWith undefined dx)

-- | Is the document part of the index?
member :: (RunCIx m, Par.MonadParallel m)
       => URI
       -> ContextIndex
       -> m Bool
member u ixx = do
  mems <- mapIxsP (DocTable.lookupByURI u . Segment.segDocs) ixx
  return (List.any isJust mems)

-- | Delete a set of documents by 'DocId'.
delete :: (RunCIx m, Par.MonadParallel m)
       => DocIdSet
       -> ContextIndex
       -> m ContextIndex
delete dIds ixx
    | DocIdSet.null dIds = return ixx
    | otherwise          = delete' dIds ixx

delete' :: (RunCIx m, Par.MonadParallel m)
        => DocIdSet
        -> ContextIndex
        -> m ContextIndex
delete' dIds ixx
  = do sm <- for (ciSegments ixx) (return . Segment.deleteDocs dIds)
       as <- Segment.activeDeleteDocs dIds (ciActiveSegment ixx)
       return ixx { ciActiveSegment = as
                  , ciSegments = sm
                  }

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (RunCIx m, Par.MonadParallel m)
                => Set URI
                -> ContextIndex
                -> m ContextIndex
deleteDocsByURI us ixx
  = do sx <- for (ciSegments ixx) (Segment.deleteDocsByURI us)
       as <- Segment.activeDeleteDocsByURI us (ciActiveSegment ixx)
       return ixx { ciSegments = sx
                  , ciActiveSegment = as
                  }

mapIxsP :: Par.MonadParallel m => (forall k. Segment k -> m a) -> ContextIndex -> m [a]
mapIxsP f ixx
  = Par.mapM f . (coerce (ciActiveSegment ixx):) . fmap snd . SegmentMap.toList . ciSegments $ ixx
{-# INLINE mapIxsP #-}
