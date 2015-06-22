{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Types where

import qualified Data.Map.Strict as Map
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import           Data.Binary
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.IntMap as IntMap

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import           Hunt.ContextIndex.Segment
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

data ContextIndex dt
  = ContextIndex { ciSegments      :: !(SegmentMap (Segment dt))
                 , ciSchema        :: !Schema
                 , ciNextSegmentId :: !SegmentId
                 , ciMergeLock     :: !MergeLock
                 }

empty :: DocTable dt => ContextIndex dt
empty
  = ContextIndex { ciSegments      = newSegmentMap
                 , ciSchema        = mempty
                 , ciNextSegmentId = SegmentId 1
                 , ciMergeLock     = mempty
                 }

newContextMap :: ContextIndex dt -> ContextMap
newContextMap
  = newContextMap' . ciSchema

mapIxs :: Monad m => (Segment dt -> m a) -> ContextIndex dt -> m [a]
mapIxs f
  = mapM f . fmap snd  .  toList . ciSegments
{-# INLINE mapIxs #-}

mapIxs' :: (Segment dt -> a) -> ContextIndex dt -> [a]
mapIxs' f
  = fmap f . fmap snd  . toList . ciSegments
{-# INLINE mapIxs' #-}

mapIxsP :: Par.MonadParallel m => (Segment dt -> m a) -> ContextIndex dt -> m [a]
mapIxsP f
  = Par.mapM f . fmap snd . toList  . ciSegments
{-# INLINE mapIxsP #-}

instance Binary dt => Binary (ContextIndex dt) where
  get = undefined
  put = undefined
