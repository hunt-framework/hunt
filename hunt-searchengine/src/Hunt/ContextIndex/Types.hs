{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Types where

import Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import Hunt.Index.Schema (Schema)
import Hunt.Segment (Segment)

import Control.DeepSeq
import Data.Aeson
import Data.Binary
import Data.Monoid
import Data.Set (Set)
import Data.Typeable

-- | The actual index type.
data ContextIndex dt
  = ContextIndex { ciActiveSegment :: !(Segment dt)
                 , ciSegments      :: !(SegmentMap (Segment dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciDirtiness     :: !Dirtiness
                 }

-- |Inserting a new segment into a ContextIndex changes its dirtiness.
--  This is used to signal Merger and Flusher to do some work.
data Dirtiness = IsDirty !(Set SegmentId)
               | NotDirty
               deriving (Eq)

instance Monoid Dirtiness where
  mempty = NotDirty
  mappend (IsDirty xs) (IsDirty ys) = IsDirty (xs <> ys)
  mappend (IsDirty xs) NotDirty     = IsDirty xs
  mappend NotDirty     (IsDirty xs) = IsDirty xs
  mappend _            _            = NotDirty

instance NFData dt => NFData (ContextIndex dt) where
  rnf ixx = rnf (ciActiveSegment ixx)
    `seq` rnf (ciSegments ixx)
    `seq` rnf (ciSchema ixx)
    `seq` rnf (ciNextSegmentId ixx)

-- | TODO: remove dummy instance
instance Binary (ContextIndex dt) where
  get = undefined
  put = undefined
