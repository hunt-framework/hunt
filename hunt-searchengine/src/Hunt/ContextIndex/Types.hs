{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Types where

import           Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import qualified Hunt.Common.SegmentMap as SegmentMap
import           Hunt.Index.Schema (Schema)
import           Hunt.Segment (Segment)

import           Data.Aeson
import           Data.Binary
import           Data.Typeable

-- | A number representing the version of the index
newtype Revision
  = Revision Int
  deriving (Binary, Enum, Eq, FromJSON, ToJSON, Ord, Show, Typeable)

-- | Settings for merging of segments.
data MergePolicy
  = MergePolicy { mpMergeFactor :: !Int
                , mpMinMerge    :: !Int
                }
  deriving (Eq, Show)

-- | A set indicating which `Segment`s are locked for merging.
newtype MergeLock
  = MergeLock { unMergeLock :: SegmentMap () }

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
  = ApplyMerge { applyMerge  :: ContextIndex dt -> ContextIndex dt }

-- | `ApplyMerge`s can be combined
instance Monoid (ApplyMerge dt) where
  mempty = ApplyMerge id
  mappend f g = ApplyMerge (applyMerge f . applyMerge g)

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

-- | The actual index type.
data ContextIndex dt
  = ContextIndex { ciSegments      :: !(SegmentMap (Segment dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciMergeLock     :: !MergeLock
                 }

-- | TODO: remove dummy instance
instance Binary (ContextIndex dt) where
  get = undefined
  put = undefined
