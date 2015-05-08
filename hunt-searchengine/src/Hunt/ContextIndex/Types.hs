{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.ContextIndex.Types where

import qualified Data.Map.Strict as Map
import           Control.Monad
import qualified Control.Monad.Parallel as Par
import           Data.Binary
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Set (Set)

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet (DocIdSet)
import           Hunt.DocTable (DocTable)
import qualified Hunt.DocTable as DocTable
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

newtype ContextMap
  = ContextMap { cxMap :: Map Context Ix.IndexImpl }
  deriving (Show)

data ContextIndex dt
  = ContextIndex { ciSegments  :: ![Segment dt]
                 , ciSchema    :: !Schema
                 }

newtype SegmentId
  = SegmentId Int
    deriving (Enum, Eq, Ord, Show)

data Segment dt
  = Segment { segId          :: !SegmentId
            , segIndex       :: !ContextMap
            , segDocs        :: !dt
            , segIsDirty     :: !Bool
            , segDeletedDocs :: !DocIdSet
            , segDeletedCxs  :: !(Set Context)
            }

newtype SnapshotId
  = SnapshotId Int
    deriving (Enum, Eq, Ord, Show)

data Snapshot
  = Snapshot { snId              :: !SnapshotId
             , snDeletedDocs     :: !DocIdSet
             , snDeletedContexts :: !(Set Context)
             , snContextMap      :: !ContextMap
             }

instance Binary dt => Binary (ContextIndex dt) where
  get = undefined
  put = undefined

empty :: DocTable dt => ContextIndex dt
empty
  = ContextIndex { ciSegments = mempty
                 , ciSchema   = mempty
                 }

mkContextMap :: Map Context Ix.IndexImpl -> ContextMap
mkContextMap m = ContextMap $! m

mapIxs :: Monad m => (Segment dt -> m a) -> ContextIndex dt -> m [a]
mapIxs f
  = mapM f . ciSegments

mapIxs' :: (Segment dt -> a) -> ContextIndex dt -> [a]
mapIxs' f
  = fmap f . ciSegments

mapIxsP :: Par.MonadParallel m => (Segment dt -> m a) -> ContextIndex dt -> m [a]
mapIxsP f
  = Par.mapM f . ciSegments

newContextMap :: Schema -> ContextMap
newContextMap = ContextMap . Map.map (newIx . ctIxImpl . cxType)
  where
    newIx :: Ix.IndexImpl -> Ix.IndexImpl
    newIx (Ix.IndexImpl i) = Ix.mkIndex (Ix.empty `asTypeOf` i)
