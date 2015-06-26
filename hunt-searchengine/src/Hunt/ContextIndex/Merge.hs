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
                , mpMinMerge    :: !Int
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

data SegmentAndLevel dt
  = SegmentAndLevel { sasLevel :: !Float
                    , sasSegId :: !SegmentId
                    , sasSeg   :: !(Segment dt)
                   }

instance Eq (SegmentAndLevel dt) where
  (SegmentAndLevel l1 sid1 _) == (SegmentAndLevel l2 sid2 _)
    = (l1 == l2) && (sid1 == sid2)

instance Ord (SegmentAndLevel dt) where
  compare (SegmentAndLevel l1 sid1 _) (SegmentAndLevel l2 sid2 _)
    = case compare l1 l2 of
        EQ -> compare sid1 sid2
        x  -> x

-- | Quantifies segments into levels.
--
quantifySegments :: Monad m
                 => (SegmentId -> Segment dt -> m Float)
                 -> SegmentMap (Segment dt)
                 -> m [SegmentAndLevel dt]
quantifySegments findLevel sx
  = forM (toList sx) $ \(sid, s) -> do
      q <- findLevel sid s
      return (SegmentAndLevel q sid s)

-- | Collects all merges in a ordered (desc. by level) list of segments.
--   Where the level is `log(sz)/log(mergeFactor)`.
collectMerges :: Monad m
              => (Float -> Bool)
              -> (Float -> Bool)
              -> [SegmentAndLevel dt]
              -> m [[SegmentAndLevel dt]]
collectMerges isMin isMax sx
  = return (collect sx [])
  where
    collect [] !acc = acc
    collect sx !acc = collect rest (viable:acc)
      where
        level = sasLevel (List.head sx)
        (inLevel, rest) = List.span (\sas -> if isMin (sasLevel sas)
                                              then sasLevel sas >= -1
                                              else sasLevel sas >= level - 1.0) sx
        viable = List.takeWhile (\sas -> if isMin (sasLevel sas)
                                          then sasLevel sas >= -1
                                          else sasLevel sas >= level - 0.75) inLevel

mkMergeDescr :: Schema
             -> SegmentId
             -> [SegmentMap (SegmentAndLevel dt)]
             -> (SegmentId, [MergeDescr dt])
mkMergeDescr schema
  = List.mapAccumL accum
  where
    accum sid sx
      = (succ sid, descr)
      where
        descr = MergeDescr sid schema (fmap sasSeg sx)

lockFromMerges :: [MergeDescr dt] -> MergeLock
lockFromMerges
  = mconcat . fmap (\(MergeDescr _ _ s) -> MergeLock (void s))

mkLock :: SegmentMap a -> MergeLock
mkLock = MergeLock . void

-- | Selects `Segment`s for merging, respecting an existing `MergeLock`.
selectMergeables :: Monad m => MergeLock -> SegmentMap a -> m (SegmentMap a)
selectMergeables (MergeLock lock) sx
  = return (difference sx lock)

-- | Given a policy and a set of `Segment`s this function decides,
--   which merges can be performed.
--
selectMerges :: (Monad m, DocTable dt)
             => MergePolicy
             -> MergeLock
             -> Schema
             -> SegmentId
             -> SegmentMap (Segment dt)
             -> m ([MergeDescr dt], MergeLock, SegmentId)
selectMerges policy lock schema nextSid segments
  = do mergeables <- selectMergeables lock segments
       quantified <- quantifySegments logNormQuantify mergeables
       allMerges  <- collectMerges isMinViableLevel (const False) (sortByLevel quantified)
       let partitions = List.filter (\p -> size p >= 2)
                        . fmap (fromList . fmap (\sas -> (sasSegId sas, sas)))
                        $ allMerges
           (nextSid', merges) = mkMergeDescr schema nextSid partitions
       return (merges, lockFromMerges merges `mappend` lock, nextSid')
  where
    norm
      = logBase (fromIntegral (mpMergeFactor policy)) . fromIntegral

    roundUp
      = max (mpMinMerge policy)

    logNormQuantify _ s
      = do sz <- segmentSize' s
           return (norm (roundUp sz))

    isMinViableLevel level
      = level <= norm (mpMinMerge policy)

    sortByLevel
      = List.sort


-- | Runs a merge. Returns an idempotent function which,
--   when applied to a `ContextIndex` makes the merged segment visible
--
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
