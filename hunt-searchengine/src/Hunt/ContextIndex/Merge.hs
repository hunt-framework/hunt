{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Merge where

import Control.Monad.IO.Class
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import Hunt.ContextIndex.Segment
import Hunt.DocTable (DocTable)
import Hunt.ContextIndex.Types
import Hunt.Utility

newtype MergeLock
  = MergeLock (Set SegmentId)
  deriving (Eq, Monoid)

newtype Merge m dt
  = Merge { runMerge :: m (MergeResult dt) }

data MergeResult dt
  = MergeResult { mrModIxx      :: ContextIndex dt -> ContextIndex dt
                , mrReleaseLock :: MergeLock -> MergeLock
                }

data TryMerge m dt
  = TryMerge !MergeLock !(Merge m dt)

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
      = ciSchema ixx

    mergedIds
      = Set.fromList (fmap segId mergeables)

    lock
      = MergeLock (lockedSegs `mappend` mergedIds)

    mergeables
      = List.filter (\s -> Set.notMember (segId s) lockedSegs) (ciSegments ixx)
