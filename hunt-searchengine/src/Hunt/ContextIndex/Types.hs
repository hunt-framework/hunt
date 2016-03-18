{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
module Hunt.ContextIndex.Types where

import           Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import qualified Hunt.Common.SegmentMap as SegmentMap
import           Hunt.ContextIndex.Segment (Segment, Kind(..))
import           Hunt.ContextIndex.Lock (SegmentLock)
import           Hunt.Index.Schema (Schema)

import           Control.DeepSeq
import           Data.Binary
import           Data.Monoid
import           Data.Set (Set)

 -- | Settings for merging of segments.
data MergePolicy =
  MergePolicy { mpMaxParallelMerges     :: Int
              , mpMergeFactor           :: Int
              , mpMinMerge              :: Int
              , mpMaxActiveSegmentLevel :: Float
              }
  deriving (Eq, Show)

data FlushPolicy =
  FlushPolicy { fpFlushDirectory :: FilePath
              }

-- | The actual index type.
data ContextIndex dt
  = ContextIndex { ciActiveSegment :: !(Segment 'Active dt)
                 , ciSegments      :: !(SegmentMap (Segment 'Frozen dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciMergePolicy   :: !MergePolicy
                 , ciSegmentLock   :: !SegmentLock
                 }

instance NFData dt => NFData (ContextIndex dt) where
  rnf ixx = rnf (ciActiveSegment ixx)
    `seq` rnf (ciSegments ixx)
    `seq` rnf (ciSchema ixx)
    `seq` rnf (ciNextSegmentId ixx)

-- | TODO: remove dummy instance
instance Binary (ContextIndex dt) where
   get = undefined
   put = undefined

-- |The ContxtIndex knows best when Segments need merging
-- or flushing. An index action can be performed asynchrously
-- as long as the ContextIndex is eventually applied to the result
-- of an index action. Invariantly, the resulting function needs
-- to be idempotent. This hopefully obsoletes the dirtiness check.
data IndexAction dt =
  IndexAction { runIxAction :: IO (ContextIndex dt -> IO (ContextIndex dt, [IndexAction dt])) }

instance Monoid (IndexAction dt) where
  mempty = IndexAction $
    return $ \ixx -> return (ixx, [])
  mappend x y = IndexAction $ do
    x' <- runIxAction x
    y' <- runIxAction y
    return $ \ixx -> do
      (ixx', ax) <- x' ixx
      (ixx'', ay) <- y' ixx'
      return (ixx'', ax <> ay)
