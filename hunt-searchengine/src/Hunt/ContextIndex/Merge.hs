{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Hunt.ContextIndex.Merge where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set

import           Hunt.ContextIndex.Segment
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable)
import           Hunt.Index.Schema
import           Hunt.Utility

data MergePolicy
  = MergePolicy { mpMergeFactor :: !Int
                }
  deriving (Eq, Show)

newtype ApplyMerge dt
  = ApplyMerge { applyMerge  :: ContextIndex dt -> ContextIndex dt }

instance Monoid (ApplyMerge dt) where
  mempty
    = ApplyMerge id
  mappend (ApplyMerge f) (ApplyMerge g)
    = ApplyMerge (f . g)

data MergeDescr dt
  = MergeDescr !SegmentId !Schema !(SegmentMap (Segment dt))

data SegmentAndSize
  = SegmentAndSize !Int !SegmentId
  deriving (Eq, Show)

instance Ord SegmentAndSize where
  compare (SegmentAndSize sz1 sid1) (SegmentAndSize sz2 sid2) =
    case compare sz1 sz2 of
      EQ -> compare sid1 sid2
      x  -> x

selectMerges :: (Functor m, Monad m, DocTable dt)
             => MergePolicy
             -> MergeLock
             -> Schema
             -> SegmentId
             -> SegmentMap (Segment dt)
             -> m ([MergeDescr dt], MergeLock, SegmentId)
selectMerges policy (MergeLock lock) schema nextSegmentId segments
  = do sas <- sortByKey <$> mapM (\(sid, s) -> do sz <- segmentSize' s
                                                  return (SegmentAndSize (roundUp sz) sid, s)
                                 ) (toList (difference segments lock))

       let partitions = filter (\p -> size p >= mpMergeFactor policy)
                        . fmap (fromList . fmap (\(SegmentAndSize _ sid, s) -> (sid, s)))
                        $ collect (mpMergeFactor policy) (mpMergeFactor policy - 1) sas []

           (nextSegmentId', descr) = List.mapAccumL (\sid p ->
                                                      (succ sid, MergeDescr sid schema p)
                                                    ) nextSegmentId partitions

           lock' = MergeLock lock <> mconcat (fmap (MergeLock . void) partitions)

       return (descr, lock', nextSegmentId')
  where
    roundUp :: Int -> Int
    roundUp x = max x 100

    sortByKey = Map.toAscList . Map.fromList

    collect !_mf  !l [] !acc = acc
    collect !mf   !l sx !acc = collect mf (l * mf) rest (p:acc)
      where
        (p, rest) = List.span (\(SegmentAndSize sz _, _) -> sz <= l) sx

runMerge :: (MonadIO m, DocTable dt) => MergeDescr dt -> m (ApplyMerge dt)
runMerge (MergeDescr segmentId schema segments)
  = do newSeg <- foldM1' (mergeSegments schema) (elems segments)
       return (newSeg `seq`
               ApplyMerge { applyMerge = applyMergedSegment segmentId segments newSeg })


applyMergedSegment :: SegmentId
                   -> SegmentMap (Segment dt)
                   -> Segment dt
                   -> ContextIndex dt
                   -> ContextIndex dt
applyMergedSegment segmentId oldSegments newSegment ixx
  = ixx { ciSegments      =
              insert segmentId newSegment' (
                difference (ciSegments ixx) oldSegments)
        , ciNextSegmentId = succ (ciNextSegmentId ixx)
        , ciMergeLock     = lock'
        }
  where
    newSegment'
      = newSegment { segDeletedCxs  = deltaDelCx
                   , segDeletedDocs = deltaDelDocs
                   }

    lock'
      = MergeLock (unMergeLock (ciMergeLock ixx) `difference` oldSegments)

    SegmentDiff !deltaDelDocs !deltaDelCx =
      mconcat (
        elems (intersectionWith segmentDiff oldSegments (ciSegments ixx)))

tryMerge :: (Functor m, Monad m, DocTable dt)
         => MergePolicy
         -> ContextIndex dt
         -> m ([MergeDescr dt], ContextIndex dt)
tryMerge policy ixx
  = do (merges, lock, nextSegmentId) <-
         selectMerges policy (ciMergeLock ixx)  (ciSchema ixx) (ciNextSegmentId ixx) (ciSegments ixx)
       let ixx' = ixx { ciMergeLock     = lock
                      , ciNextSegmentId = nextSegmentId
                      }
       return (merges, ixx')
