{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnboxedTuples             #-}
module Hunt.ContextIndex.Flush(
    runFlush
  , FlushPolicy(..)
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId (DocId)
import           Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.Common.SegmentMap (SegmentId, SegmentMap)
import qualified Hunt.Common.SegmentMap as SegmentMap
import           Hunt.ContextIndex.Types
import           Hunt.DocTable (DocTable, DValue)
import qualified Hunt.DocTable as DocTable
import           Hunt.ContextIndex.Segment (Segment (..), Kind(..))
import qualified Hunt.ContextIndex.Segment as Segment

import qualified Data.ByteString.Lazy as LByteString
import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           System.FilePath
import           System.IO
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

-- | `Revision` holds the state of an index.

data Revision =
  Revision { revSegments :: !(SegmentMap (DocIdSet, Set Context))  }

-- | A `Flush` describes the operations to persist the index.
data Flush =
  forall dt. DocTable dt => Flush { flsAddSeg :: SegmentMap (Segment 'Frozen dt) -- | ^ Write a new `Segment` (and add a SEGMENT_N file)
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
runFlush :: (MonadIO m, DocTable dt, Binary.Binary (DValue dt)) =>
            FlushPolicy -> SegmentId -> Segment 'Frozen dt -> m (ContextIndex dt -> ContextIndex dt)
runFlush policy sid seg = do
  !dix <- writeDocTable policy sid seg
  return $ \ixx ->
    ixx { ciSegments =
            SegmentMap.insertWith (\_ s -> s { segDocs = DocTable.empty }) sid seg (ciSegments ixx)
        }

-- |A DocTableIndex represents an on-disk DocTable. dtiDocIds is oredered strictly
-- monotone. While dtiDocInfo contains the on-disk offset and size of the document.
-- This structure allows document access with exactly one disk-seek.
data DocTableIndex =
  DTI { dtiDocIds :: !(UVector.Vector DocId)
      , dtiDocInfo :: !(UVector.Vector (Word64, Word64))
      }

writeDocTable :: (MonadIO m, Binary.Binary (DValue dt), DocTable dt) =>
                 FlushPolicy -> SegmentId -> Segment 'Frozen dt -> m DocTableIndex
writeDocTable policy sid seg = liftIO $ do

  withFile dtIxFile WriteMode $ \ix -> do
    withFile dtDocFile WriteMode $ \docs -> do

      -- Don't access DocTable directly, as it could be an already
      -- flushed Segment so we don't clutter memory
      docIds <- Segment.segmentDocIds seg

      -- A vector representation for our new new DocTable index.
      -- We use two vectors, one for the strictly ordered DocIds
      -- and one for the (offset, size) info for the disk seek.
      let numDocs = DocIdSet.size docIds
      mDtIx <- UMVector.unsafeNew numDocs
      mDtInfo <- UMVector.unsafeNew numDocs

      foldM_ (\(offset, i) did -> do
                 let putDix = LByteString.hPut ix . Binary.runPut
                 Just doc <- Segment.lookupDocument did seg

                 let docEntry = Binary.runPut (Binary.put doc)
                     size = fromIntegral $ LByteString.length docEntry

                 putDix $ do
                   Binary.putWord64be offset
                   Binary.putWord64be size
                 LByteString.hPut docs docEntry

                 UMVector.unsafeWrite mDtIx i did
                 UMVector.unsafeWrite mDtInfo i (offset, size)

                 return (offset + size, i + 1)
                 ) (0, 0) (DocIdSet.toList docIds)

      dtIx <- UVector.unsafeFreeze mDtIx
      dtInfo <- UVector.unsafeFreeze mDtInfo

      return DTI { dtiDocIds = dtIx
                 , dtiDocInfo = dtInfo
                 }
  where
    dtIxFile = fpFlushDirectory policy </> show sid <.> "dix"
    dtDocFile = fpFlushDirectory policy </> show sid <.> "dt"
