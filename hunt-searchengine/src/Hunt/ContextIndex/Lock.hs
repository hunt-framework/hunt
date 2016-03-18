{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Lock where

import           Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import qualified Hunt.Common.SegmentMap as SegmentMap

import           Control.DeepSeq

-- | A simple structure for remembering which Segments
-- may not be touched by the merger.
newtype SegmentLock = SegmentLock { unSegmentLock :: SegmentMap () }
                      deriving (NFData)

instance Monoid SegmentLock where
  mempty = SegmentLock mempty
  mappend (SegmentLock x) (SegmentLock y) = SegmentLock (x `mappend` y)

singleton :: SegmentId -> SegmentLock
singleton sid = SegmentLock (SegmentMap.insert sid () mempty)

lock :: SegmentId -> SegmentLock -> SegmentLock
lock sid (SegmentLock sx) = SegmentLock (SegmentMap.insert sid () sx)

release :: SegmentId -> SegmentLock -> SegmentLock
release sid (SegmentLock sx) = SegmentLock (SegmentMap.delete sid sx)

release' :: SegmentMap a -> SegmentLock -> SegmentLock
release' sx (SegmentLock lx) = SegmentLock (SegmentMap.difference lx sx)

filterUnlocked :: SegmentMap a -> SegmentLock -> SegmentMap a
filterUnlocked sx (SegmentLock lx) = SegmentMap.difference sx lx

fromSegmentMap :: SegmentMap a -> SegmentLock
fromSegmentMap sx = SegmentLock (fmap (const ()) sx)
