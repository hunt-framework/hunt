{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}
module Hunt.ContextIndex.Merge (
    MergePolicy(..)
  , MergeDescr

  , lockFromDescrs

  , quantify
  , quantify'

  , tryMerge
  , runMerge
  ) where

import           Hunt.Common.SegmentMap (SegmentId (..), SegmentMap)
import qualified Hunt.Common.SegmentMap as SegmentMap
import           Hunt.ContextIndex.Types
import           Hunt.ContextIndex.Lock (SegmentLock)
import qualified Hunt.ContextIndex.Lock as Lock
import           Hunt.DocTable (DocTable)
import           Hunt.Index.Schema
import           Hunt.ContextIndex.Segment
import qualified Hunt.ContextIndex.Segment as Segment
import           Hunt.Utility

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List as List
import           Data.Monoid
import           Data.Ord

-- | A description of a merge.
data MergeDescr dt
  = MergeDescr { -- | Id of the new `Segment`
                 mdSegId  :: !SegmentId
                 -- | Schema used to merge the `Segment`s
               , mdSchema :: !Schema
                 -- | Actual `Segment`s to merge.
               , mdSegs   :: !(SegmentMap (Segment 'Frozen dt))
               }

instance Show (MergeDescr dt) where
  show (MergeDescr sid _ m)
    = "MergeDescr { merging = "
      ++ show (SegmentMap.keys m)
      ++ ", to = "
      ++ show sid
      ++ " }"

-- |A measure in (0..1.0).
type Level = Float

-- | A convenience data structure for storing numbers relevant
--   for the merge descision.
data SegmentAndLevel dt =
  SegmentAndLevel { -- | Discrete level in our geometric series.
                    sasLevel :: !Level
                    -- | Refers to Segment being merged.
                  , sasSegId :: !SegmentId
                    -- | The actual `Segment` value.
                  , sasSeg   :: !(Segment 'Frozen dt)
                  }

-- | Two `SegmentAndLevel` are equal if they refer to the same
--   `Segment` and are on the same level.
instance Eq (SegmentAndLevel dt) where
  (SegmentAndLevel l1 sid1 _) == (SegmentAndLevel l2 sid2 _)
    = (l1 == l2) && (sid1 == sid2)

-- | Two `SegmentAndLevel` are ordered by their respective levels
--   and by their `SegmentId`s.
instance Ord (SegmentAndLevel dt) where
  compare (SegmentAndLevel l1 sid1 _) (SegmentAndLevel l2 sid2 _)
    = compare l1 l2 <> compare sid1 sid2 -- Ordering forms a Monoid

-- | Quantifies segments into levels.
quantifySegments :: Monad m
                 => (Segment 'Frozen dt -> m Level)
                 -> SegmentMap (Segment 'Frozen dt)
                 -> m [SegmentAndLevel dt]
quantifySegments findLevel sx
  = forM (SegmentMap.toList sx) $ \(sid, s) -> do
      lvl <- findLevel s
      return (SegmentAndLevel lvl sid s)

-- | Collects all merges in a ordered (desc. by level) list of segments.
--   Where the level is `log(sz)/log(mergeFactor)`.
collectMerges :: Monad m
              => (Float -> Bool)
              -> (Float -> Bool)
              -> [SegmentAndLevel dt]
              -> m [[SegmentAndLevel dt]]
collectMerges isMin _isMax segments
  = return (collect segments [])
  where
    collect [] !acc = acc
    collect sx !acc = collect rest (viable:acc)
      where
        level = sasLevel (List.head sx)
        (inLevel, rest) = List.span (\sas -> if isMin (sasLevel sas)
                                              then sasLevel sas >= -1
                                              else sasLevel sas >= level - 1.0) sx

        -- We have identified every `Segment` on a level. Do not merge all at once
        -- instead take only 75% (this quite arbitrary but prevents too big merges after all)
        viable = List.takeWhile (\sas -> if isMin (sasLevel sas)
                                          then sasLevel sas >= -1
                                          else sasLevel sas >= level - 0.75) inLevel

-- | Creates the final `MergeDescr`s from the levels. `SegmentId` is
--   threaded through.
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
        descr = MergeDescr { mdSegId  = sid
                           , mdSchema = schema
                           , mdSegs   = fmap sasSeg sx
                           }

-- | Create a `Mergelock` from a bunch of merge descriptions.
lockFromDescrs :: [MergeDescr dt] -> SegmentLock
lockFromDescrs
  = mconcat . fmap (Lock.fromSegmentMap . mdSegs)

-- | Selects `Segment`s for merging, respecting an existing `MergeLock`.
--   This is monadic for the case we need to look at some other criterea
--   other than the `MergeLock`.
selectMergeables :: Monad m => SegmentLock -> SegmentMap a -> m (SegmentMap a)
selectMergeables lock sx
  = return $ Lock.filterUnlocked sx lock

-- |Quantify and normalize a measure.
quantify :: MergePolicy -> Int -> Level
quantify policy sz = logNormQuantify sz
  where
    -- Since we are using a geometric series we normalize our costs (size of segment)
    -- logarithmacally to the base of the merge factor.
    norm :: Int -> Level
    norm = logBase (fromIntegral (mpMergeFactor policy)) . fromIntegral

    -- Round up too small sizes  to a size where merging is more efficient.
    roundUp :: Int -> Int
    roundUp = max (mpMinMerge policy)

    -- Normalize a the cost for merge (size of segment)
    logNormQuantify :: Int -> Level
    logNormQuantify n = norm (roundUp n)

-- | Quantify something convertible to int.
quantify' :: Monad m => (a -> m Int) -> MergePolicy -> a -> m Level
quantify' f pol x = quantify pol <$> f x

-- | Given a policy and a set of `Segment`s this function decides,
--   which merges can be performed.
selectMerges :: (Monad m, DocTable dt)
             => MergePolicy
             -> SegmentLock
             -> Schema
             -> SegmentId
             -> SegmentMap (Segment 'Frozen dt)
             -> m ([MergeDescr dt], SegmentLock, SegmentId)
selectMerges policy lock schema nextSid segments
  = do -- select any segment not locked by mergelock
       mergeables <- selectMergeables lock segments
       -- assign the segments to levels in our geometric series
       quantified <- quantifySegments (quantify' segmentSize' policy) mergeables
       -- collect all `Segments` which are equal by our definition of level.
       allMerges  <- collectMerges isMinViableLevel (const False) (sortByLevel quantified)
       -- Filter out single level merges
       let partitions = List.filter (\p -> SegmentMap.size p >= 2)
                        . fmap (SegmentMap.fromList . fmap (\sas -> (sasSegId sas, sas)))
                        $ allMerges
           (nextSid', merges) = mkMergeDescr schema nextSid partitions
       return (merges, lockFromDescrs merges `mappend` lock, nextSid')
  where
    -- Is given level below the minimal merge size
    isMinViableLevel level
      = level <= quantify policy (mpMinMerge policy)

    -- Sort a list in descending order.
    -- Used to group ~equal~ segments next to each other.
    sortByLevel
      = List.sortBy (comparing Down)

-- | Runs a merge. Returns an idempotent function which,
--   when applied to a `ContextIndex` makes the merged segment visible
runMerge' :: (MonadIO m, DocTable dt) => MergeDescr dt -> m (Segment 'Frozen dt)
runMerge' (MergeDescr _segmentId schema segm) =
  go (head segments) (tail segments)
  where
    segments = SegmentMap.elems segm
    go acc [] = return acc
    go acc (s:sx) = do
      acc' <- go acc sx
      mergeSegments schema s acc'

-- | Selects segments viable to merge but don't actually do the merge
--   because it can be a quite costly operation. Marks segments as
--   merging.
tryMerge :: (Functor m, Monad m, DocTable dt)
         => ContextIndex dt
         -> m ([MergeDescr dt], ContextIndex dt)
tryMerge ixx
  = do (merges, lock', nextSegmentId) <-
         selectMerges policy lock (ciSchema ixx) (ciNextSegmentId ixx) (ciSegments ixx)
       let ixx' = ixx { ciNextSegmentId = nextSegmentId
                      , ciSegmentLock = lock'
                      }
       return (merges, ixx')
         where
           lock = ciSegmentLock ixx
           policy = ciMergePolicy ixx

-- | Takes a list of merge descriptions and performs the merge of the
--   segments. Returns an idempotent function which can be applied to the
--   `ContextIndex`. This way, the costly merge can be done asynchronously.
runMerge :: (MonadIO m, DocTable dt) => MergeDescr dt
            -> m (ContextIndex dt -> (SegmentId, Segment 'Frozen dt, ContextIndex dt))
runMerge descr
  = do !newSeg <- runMerge' descr
       return $ \ixx ->
         let
           ixx' = applyMergedSegment (mdSegId descr) (mdSegs descr) newSeg ixx
         in (mdSegId descr, newSeg, ixx')
-- | Since merging can happen asynchronously, we have to account for documents
--   and contexts deleted while we were merging the segments.
applyMergedSegment :: SegmentId
                   -> SegmentMap (Segment 'Frozen dt)
                   -> Segment 'Frozen dt
                   -> ContextIndex dt
                   -> ContextIndex dt
applyMergedSegment segmentId oldSegments newSegment ixx
  = ixx { ciSegments      =
              SegmentMap.insertWith (const id) segmentId newSegment' (
                SegmentMap.difference (ciSegments ixx) oldSegments)
        , ciSegmentLock = Lock.release' oldSegments (ciSegmentLock ixx)
        }
  where
    newSegment'
      = Segment.deleteDocs deltaDelDocs
        . Segment.deleteContexts deltaDelCx
        $ newSegment

    SegmentDiff deltaDelDocs deltaDelCx
      = Segment.diff' oldSegments (ciSegments ixx)
