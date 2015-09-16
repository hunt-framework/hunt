{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE RankNTypes                 #-}
module Hunt.ContextIndex.Merge (
    MergePolicy(..)
  , MergeLock
  , MergeDescr

  , tryMerge
  , runMerge
  , applyMerge
  ) where

import           Hunt.Common.SegmentMap  (SegmentId (..), SegmentMap)
import qualified Hunt.Common.SegmentMap  as SegmentMap
import           Hunt.ContextIndex.Types
import           Hunt.DocTable           (DocTable)
import           Hunt.Index.Schema
import           Hunt.Segment
import qualified Hunt.Segment            as Segment
import           Hunt.Utility

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.List               as List
import           Data.Ord

-- | Settings for merging of segments.
data MergePolicy =
  MergePolicy { mpMaxParallelMerges :: Int
              , mpMergeFactor       :: Int
              , mpMinMerge          :: Int
              }
  deriving (Eq, Show)

-- | A set indicating which `Segment`s are locked for merging.
newtype MergeLock = MergeLock (SegmentMap ())
                    deriving (NFData)

instance Show MergeLock where
  show (MergeLock m) = show (SegmentMap.keys m)

-- | Locks can be combined.
instance Monoid MergeLock where
  mempty
    = MergeLock SegmentMap.empty
  mappend (MergeLock m1) (MergeLock m2)
    = MergeLock (SegmentMap.unionWith (\_ _ -> ()) m1 m2)

-- | Represents an idempotent function, applying a
--   merged `Segment` to the `ContextIndex`.
newtype ApplyMerge dt
  = ApplyMerge { applyMerge :: MergeLock -> ContextIndex dt -> (ContextIndex dt, MergeLock) }

-- | `ApplyMerge`s can be combined
instance Monoid (ApplyMerge dt) where
  mempty = ApplyMerge (\lock ixx -> (ixx, lock))
  mappend f g = ApplyMerge (\lock ixx ->
                              let (ixx', lock') = applyMerge g lock ixx
                                  in applyMerge f lock' ixx'
                           )

-- | A description of a merge.
data MergeDescr dt
  = MergeDescr { mdSegId  :: !SegmentId -- ^ Id of the new `Segment`
               , mdSchema :: !Schema    -- ^ Schema used to merge the `Segment`s
               , mdSegs   :: !(SegmentMap (Segment dt)) -- ^ Actual `Segment`s to merge.
               }

instance Show (MergeDescr dt) where
  show (MergeDescr sid _ m)
    = "MergeDescr { merging = "
      ++ show (SegmentMap.keys m)
      ++ ", to = "
      ++ show sid
      ++ " }"

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
  = forM (SegmentMap.toList sx) $ \(sid, s) -> do
      q <- findLevel sid s
      return (SegmentAndLevel q sid s)

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
        descr = MergeDescr { mdSegId  = sid
                           , mdSchema = schema
                           , mdSegs   = fmap sasSeg sx
                           }

lockFromMerges :: [MergeDescr dt] -> MergeLock
lockFromMerges
  = mconcat . fmap (\(MergeDescr _ _ s) -> MergeLock (void s))

-- | Selects `Segment`s for merging, respecting an existing `MergeLock`.
selectMergeables :: Monad m => MergeLock -> SegmentMap a -> m (SegmentMap a)
selectMergeables (MergeLock lock) sx
  = return (SegmentMap.difference sx lock)

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
       let partitions = List.filter (\p -> SegmentMap.size p >= 2)
                        . fmap (SegmentMap.fromList . fmap (\sas -> (sasSegId sas, sas)))
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
      = List.sortBy (comparing Down)


-- | Runs a merge. Returns an idempotent function which,
--   when applied to a `ContextIndex` makes the merged segment visible
--
runMerge' :: (MonadIO m, DocTable dt) => MergeDescr dt -> m (Segment dt)
runMerge' (MergeDescr _segmentId schema segments)
  = do foldM1' (mergeSegments schema) (SegmentMap.elems segments)

releaseLock :: MergeLock -> SegmentMap a -> MergeLock
releaseLock (MergeLock lock) x
  = MergeLock (SegmentMap.difference lock x)

-- | Selects segments viable to merge but don't actually do the merge
--   because it can be a quite costly operation. Marks segments as
--   merging.
tryMerge :: (Functor m, Monad m, DocTable dt)
         => MergePolicy
         -> MergeLock
         -> ContextIndex dt
         -> m ([MergeDescr dt], MergeLock, ContextIndex dt)
tryMerge policy lock ixx
  = do (merges, lock', nextSegmentId) <-
         selectMerges policy lock (ciSchema ixx) (ciNextSegmentId ixx) (ciSegments ixx)
       let ixx' = ixx { ciNextSegmentId = nextSegmentId }
       return (merges, lock', ixx')

-- | Takes a list of merge descriptions and performs the merge of the
--   segments. Returns an idempotent function which can be applied to the
--   `ContextIndex`. This way, the costly merge can be done asynchronously.
runMerge :: (MonadIO m, DocTable dt) => MergeDescr dt -> m (ApplyMerge dt)
runMerge descr
  = do newSeg <- runMerge' descr
       rnf newSeg `seq` return (
         ApplyMerge (applyMergedSegment (mdSegId descr) (mdSegs descr) newSeg))

-- | Since merging can happen asynchronously, we have to account for documents
--   and contexts deleted while we were merging the segments.
applyMergedSegment :: SegmentId
                   -> SegmentMap (Segment dt)
                   -> Segment dt
                   -> MergeLock
                   -> ContextIndex dt
                   -> (ContextIndex dt, MergeLock)
applyMergedSegment segmentId oldSegments newSegment lock ixx
  = (ixx { ciSegments      =
              SegmentMap.insertWith (const id) segmentId newSegment' (
                SegmentMap.difference (ciSegments ixx) oldSegments)
        , ciNextSegmentId = succ (ciNextSegmentId ixx)
        }, releaseLock lock oldSegments)
  where
    newSegment'
      = Segment.deleteDocs deltaDelDocs
        . Segment.deleteContexts deltaDelCx
        $ newSegment

    SegmentDiff deltaDelDocs deltaDelCx
      = Segment.diff' oldSegments (ciSegments ixx)
