{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , createDocTableFromPartition  -- only used in tests
  , unionDocTables               -- only used in tests
  , modifyWithDescription
  , delete
  , deleteDocsByURI
  , member

    -- * Documents
  , lookupDocumentByURI
  , lookupDocument
  , selectDocuments

  , ContextIndex
  , Merge(..)
  , MergeResult(..)
  , TryMerge(..)
  , MergeLock
  , tryMerge
  , commit
  , status
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Delete
import           Hunt.ContextIndex.Documents
import           Hunt.ContextIndex.Insert
import           Hunt.ContextIndex.Search
import           Hunt.ContextIndex.Segment
import           Hunt.ContextIndex.Status
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema
import           Hunt.Utility

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set

insertContext :: Context -> Ix.IndexImpl -> ContextSchema
                 -> ContextIndex dt -> ContextIndex dt
insertContext cx ix schema ixx
  = ixx { ciSchema = Map.insertWith (const id) cx schema (ciSchema ixx) }

deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciSegments = mapIxs' (segmentDeleteContext cx) ixx
        , ciSchema   = Map.delete cx (ciSchema ixx)
        }

defaultContexts :: ContextIndex dt -> [Context]
defaultContexts
  = Map.keys . Map.filter cxDefault . ciSchema

contexts :: ContextIndex dt -> [Context]
contexts
  = Map.keys . ciSchema

contextsM :: Monad m => ContextIndex dt -> m [Context]
contextsM
  = return . contexts

hasContext :: Context -> ContextIndex dt -> Bool
hasContext cx
  = Map.member cx . ciSchema

hasContextM :: Monad m => Context -> ContextIndex dt -> m Bool
hasContextM cx
  = return . hasContext cx

schema :: ContextIndex dt -> Schema
schema = ciSchema

-- TODO: does this make sense? CLEANUP!
-- | Returns a `TryMerge` action. Since merging itself can be costly `tryMerge`
--   returns a monadic action which can be executed asynchronously. It returns
--   an idempotent function which can be used to modify the `ContextIndex`
--   afterwards to make the merged segment visible and to delete old segments.
--   Bundled with  the action there is a `MergeLock` which is used
--   to ensure different merges don't touch non exclusive `Segment`s. Simply
--   pass an empty `MergeLock` if you don't own one yet.
tryMerge :: (Monad m, MonadIO n, DocTable dt)
            => MergeLock -> ContextIndex dt -> m (TryMerge n dt)
tryMerge (MergeLock lockedSegs) ixx
  = do return (TryMerge lock (Merge mergeAction))
  where
    mergeAction
      = do case mergeables of
            [] -> return (MergeResult id id)
            xs -> segmentMerge xs

    segmentMerge segs
      = do seg <- foldM1' (mergeSegments schema') segs

           -- | TODO: ModIxx needs to diff against deletedDocs/deletedContexts in
           --         ixx', since they could have changed since the time of `tryMerge`
           --         This isn't a problem since segment values increase monotonically
           --         (always add, never remove any items from the sets of deleted docs and contexts)
           let modIxx ixx'
                 = let sx = List.filter (\s -> Set.notMember (segId s) mergedIds) (ciSegments ixx')
                   in ixx' { ciSegments   = seg : sx }
               modLock (MergeLock m)
                 = MergeLock (Set.difference m mergedIds)

           return MergeResult { mrModIxx      = modIxx
                              , mrReleaseLock = modLock
                              }
    schema'
      = schema ixx

    mergedIds
      = Set.fromList (fmap segId mergeables)

    lock
      = MergeLock (lockedSegs `mappend` mergedIds)

    mergeables
      = List.filter (\s -> Set.notMember (segId s) lockedSegs) (ciSegments ixx)

-- | Flushes all dirty and not yet written `Segment`s to the index directory
--
commit :: (Binary dt, DocTable dt, MonadIO m) => FilePath -> ContextIndex dt -> m (ContextIndex dt)
commit dir ixx
  = do segments' <- mapM commitSeg (ciSegments ixx)
       return ixx { ciSegments   = segments'
                  }
  where
    commitSeg s
      = do when (isUnstaged s) $
             commitSegment dir s
           when (isDirty s) $
             commitDirtySegment dir s
           return s { segState = SegClean }
      where
            isUnstaged s = st == SegUncommited || st == SegDirtyAndUncommited
              where
                st = segState s

            isDirty s = st == SegDirty || st == SegDirtyAndUncommited
              where
                st = segState s
