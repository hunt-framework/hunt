{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

data ApplyMerge dt
  = ApplyMerge { applyMerge  :: ContextIndex dt -> ContextIndex dt
               , releaseLock :: MergeLock -> MergeLock
               }

instance Monoid (ApplyMerge dt) where
  mempty
    = ApplyMerge id id
  mappend (ApplyMerge f1 g1) (ApplyMerge f2 g2)
    = ApplyMerge (f1 . f2) (g1 . g2)

data MergeDescr dt
  = MergeDescr !Schema !(SegmentMap (Segment dt))

type Size = Int

data SegmentAndSize
  = SegmentAndSize !Size !SegmentId
  deriving (Eq)

instance Ord SegmentAndSize where
  compare (SegmentAndSize sz1 sid1) (SegmentAndSize sz2 sid2) =
    case compare sz1 sz2 of
      EQ -> compare sid1 sid2
      x  -> x

selectMerges :: (Functor m, Monad m, DocTable dt)
             => MergePolicy
             -> MergeLock
             -> Schema
             -> SegmentMap (Segment dt)
             -> m ([MergeDescr dt], MergeLock)
selectMerges policy (MergeLock lock) schema segments
  = do sas <- sortByKey <$> mapM (\(sid, s) -> do sz <- segmentSize s
                                                  return (SegmentAndSize sz sid, s)
                                 ) (toList (difference segments lock))

       let partitions = filter (\p -> size p >= mpMergeFactor policy)
                        . fmap (fromList . fmap (
                                    \(SegmentAndSize _ sid, s) -> (sid, s)))
                        $ collect levels sas []

           descr      = fmap (MergeDescr schema) partitions
           lock'      = MergeLock lock <> mconcat (fmap (MergeLock . void) partitions)

       return (descr, lock')
  where
    sortByKey = Map.toAscList . Map.fromList

    -- Geometric series
    levels :: [Int]
    levels = [ round ((r - 1)*r**(k - 1)) | k <- [1..],
               let r = fromIntegral (mpMergeFactor policy) ]

    collect _        []  acc = acc
    collect (lvl:lx) sas acc = collect lx rest (p:acc)
      where
        (p, rest) = List.break (
          \(SegmentAndSize sz _, _) -> sz <= lvl) sas

runMerge :: (MonadIO m, DocTable dt) => MergeDescr dt -> m (ApplyMerge dt)
runMerge (MergeDescr schema segments)
  = do newSeg <- foldM1' (mergeSegments schema) (elems segments)
       return (newSeg `seq` ApplyMerge { applyMerge  = applyMergedSegment segments newSeg
                                       , releaseLock = const mempty
                                       })

applyMergedSegment :: SegmentMap (Segment dt)
                   -> Segment dt
                   -> ContextIndex dt
                   -> ContextIndex dt
applyMergedSegment oldSegments newSegment ixx
  = ixx { ciSegments      =
              insert (ciNextSegmentId ixx) newSegment' (
                difference (ciSegments ixx) oldSegments)
        , ciNextSegmentId = succ (ciNextSegmentId ixx)
        , ciMergeLock = lock'
        }
  where
    newSegment'
      = newSegment { segDeletedCxs  = deltaDelCx
                   , segDeletedDocs = deltaDelDocs
                   }

    MergeLock currLock = ciMergeLock ixx

    lock'
      = MergeLock (currLock `difference` oldSegments)

    SegmentDiff deltaDelDocs deltaDelCx =
      mconcat (
        elems (intersectionWith segmentDiff oldSegments (ciSegments ixx)))

tryMerge :: (Functor m, Monad m, DocTable dt)
         => MergeLock
         -> ContextIndex dt
         -> m ([MergeDescr dt], MergeLock)
tryMerge lock ixx
  = selectMerges (MergePolicy 3) lock (ciSchema ixx) (ciSegments ixx)
