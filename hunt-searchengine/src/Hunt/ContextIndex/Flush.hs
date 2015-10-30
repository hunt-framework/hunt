{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnboxedTuples             #-}
module Hunt.ContextIndex.Flush where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocIdSet    (DocIdSet)
import qualified Hunt.Common.DocIdSet    as DocIdSet
import           Hunt.Common.SegmentMap  (SegmentMap)
import qualified Hunt.Common.SegmentMap  as SegmentMap
import           Hunt.ContextIndex       (ContextIndex)
import           Hunt.ContextIndex.Types
import           Hunt.DocTable           (DocTable)
import           Hunt.Segment            (Segment (..))

import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Set                (Set)
import qualified Data.Set                as Set

data FlushPolicy =
  FlushPolicy { fpFlushDirectory :: FilePath
              }

-- | `Revision` holds the state of an index.

data Revision =
  Revision { revSegments :: !(SegmentMap (DocIdSet, Set Context))  }

-- | A `Flush` describes the operations to persist the index.
data Flush =
  forall dt. DocTable dt => Flush { flsAddSeg :: SegmentMap (Segment dt) -- | ^ Write a new `Segment` (and add a SEGMENT_N file)
                                  , flsDelSeg :: SegmentMap ()           -- | ^ Delete `Segment`
                                  , flsUpdDel :: SegmentMap (DocIdSet, Set Context) -- | ^ Write delete-set
                                  }

data ApplyFlush  =
  forall dt. ApplyFlush { apply :: ContextIndex dt -> ContextIndex dt  }

mkRevision :: (Monad m, DocTable dt) => ContextIndex dt -> m Revision
mkRevision ixx = return (Revision segInfo)
  where
    segInfo = fmap (segDeletedDocs &&& segDeletedCxs) (ciSegments ixx)

-- | `diff` takes a `Revision` and a `ContextIndex` and creates a `Flush` based
--   on the difference of the two.
delta :: (Monad m, DocTable dt) => Revision -> ContextIndex dt -> m (Flush, Revision)
delta (Revision old) new = do
  rev' <- mkRevision new
  return (flush, rev')
  where
    flush = Flush { flsAddSeg = SegmentMap.difference (ciSegments new) old
                  , flsDelSeg = mempty
                  , flsUpdDel = mempty --SegmentMap.intersectionWith diffDel (ciSegments new) (ciSegments old)
                  }
    diffDel ns os
      | delDocsEq && delCxsEq = Nothing
      | otherwise             = Just (segDeletedDocs ns, segDeletedCxs ns)
      where
        delDocsEq = segDeletedDocs ns == segDeletedDocs os
        delCxsEq  = segDeletedCxs ns == segDeletedCxs os

-- | Runs a `Flush` and writes files to the index directory. This operation is atomic.
runFlush :: MonadIO m => FlushPolicy -> Flush -> m ApplyFlush
runFlush _ _ = return (ApplyFlush id)

applyFlush :: DocTable dt => ApplyFlush -> ContextIndex dt -> (ContextIndex dt, ())
applyFlush f ixx = (ixx, ())
