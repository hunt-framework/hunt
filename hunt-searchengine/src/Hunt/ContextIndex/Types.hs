{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Types where

import Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import Hunt.Index.Schema (Schema)
import Hunt.Segment (Segment)

import Control.DeepSeq
import Data.Aeson
import Data.Binary
import Data.Typeable

-- | The actual index type.
data ContextIndex dt
  = ContextIndex { ciActiveSegment :: !(Segment dt)
                 , ciSegments      :: !(SegmentMap (Segment dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
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
