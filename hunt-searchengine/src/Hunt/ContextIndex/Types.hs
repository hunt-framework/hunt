{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hunt.ContextIndex.Types where

import           Hunt.ContextIndex.Segment          (Kind (..), Segment)
import           Hunt.ContextIndex.Types.Lock       (SegmentLock)
import           Hunt.ContextIndex.Types.SegmentMap (SegmentId, SegmentMap)
import           Hunt.Index.Schema                  (Schema)

import           Control.DeepSeq
import           Data.Binary
import           Data.Monoid


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
data ContextIndex
  = ContextIndex { ciActiveSegment :: !(Segment 'Active)
                 , ciSegments      :: !(SegmentMap (Segment 'Frozen))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciMergePolicy   :: !MergePolicy
                 , ciSegmentLock   :: !SegmentLock
                 }

instance NFData ContextIndex where
  rnf ixx = rnf (ciActiveSegment ixx)
    `seq` rnf (ciSegments ixx)
    `seq` rnf (ciSchema ixx)
    `seq` rnf (ciNextSegmentId ixx)

-- | TODO: remove dummy instance
instance Binary ContextIndex where
   get = undefined
   put = undefined

-- |The ContxtIndex knows best when Segments need merging
-- or flushing. An index action can be performed asynchrously
-- as long as the ContextIndex is eventually applied to the result
-- of an index action. Invariantly, the resulting function needs
-- to be idempotent.
data IndexAction  = IndexAction {
  runIxAction :: IO (ContextIndex -> IO (ContextIndex, [IndexAction]))
  }

instance Monoid IndexAction where
  mempty = IndexAction $
    return $ \ixx -> return (ixx, [])
  mappend x y = IndexAction $ do
    x' <- runIxAction x
    y' <- runIxAction y
    return $ \ixx -> do
      (ixx', ax) <- x' ixx
      (ixx'', ay) <- y' ixx'
      return (ixx'', ax <> ay)
