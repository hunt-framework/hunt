{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hunt.ContextIndex.Types where

import           Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import qualified Hunt.Common.SegmentMap as SegmentMap
import           Hunt.Index.Schema (Schema)
import           Hunt.Segment (Segment)

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

-- | A set indicating which `Segment`s are locked for merging.
newtype MergeLock = MergeLock (SegmentMap ())
                  deriving (NFData)

instance Show MergeLock where
  show (MergeLock m) = show (SegmentMap.keys m)

-- | Locks can be combined.
instance Monoid MergeLock where
  mempty = MergeLock mempty
  mappend (MergeLock m1) (MergeLock m2) = MergeLock (
    SegmentMap.unionWith (\_ _ -> ()) m1 m2)

-- | The actual index type.
data ContextIndex dt
  = ContextIndex { ciActiveSegment :: !(Segment dt)
                 , ciSegments      :: !(SegmentMap (Segment dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciDirtiness     :: !Dirtiness -- ^ TODO: remove
                 , ciMergePolicy   :: !MergePolicy
                 , ciMergeLock     :: !MergeLock -- ^ TODO: remove
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
  IndexAction { runIxAction :: IO (ContextIndex dt -> ContextIndex dt) }

instance Monoid (IndexAction dt) where
  mempty = IndexAction (return id)
  mappend x y = IndexAction $ (.) <$> runIxAction x <*> runIxAction y

-- |Inserting a new segment into a ContextIndex changes its dirtiness.
-- This is used to signal Merger and Flusher to do some work.
data Dirtiness = IsDirty !(Set SegmentId)
               | NotDirty
               deriving (Eq)

instance Monoid Dirtiness where
  mempty = NotDirty
  mappend (IsDirty xs) (IsDirty ys) = IsDirty (xs <> ys)
  mappend (IsDirty xs) NotDirty     = IsDirty xs
  mappend NotDirty     (IsDirty xs) = IsDirty xs
  mappend _            _            = NotDirty
