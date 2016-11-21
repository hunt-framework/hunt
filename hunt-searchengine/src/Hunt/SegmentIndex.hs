{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex (
    SegIxRef

  , openOrNewSegmentIndex
  , AccessMode(..)
  , AtRevision(..)
  , IndexOpenError(..)

  , insertContext
  , deleteContext

  , newWriter
  , insertDocuments
  , closeWriter
  ) where

import           Hunt.Common.ApiDocument            (ApiDocument)
import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import qualified Hunt.SegmentIndex.IndexWriter      as IndexWriter
import           Hunt.SegmentIndex.Open
import qualified Hunt.SegmentIndex.Store            as Store
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.SegmentId
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Control.Concurrent.MVar
import qualified Data.Map.Strict                    as Map

-- | Inserts a 'Context' into the index.
--  /Note/: Does nothing if the context already exists.
insertContext :: SegIxRef
              -> Context
              -> ContextSchema
              -> IO ()
insertContext segmentIndexRef context contextSchema = do
  withSegmentIndex_ segmentIndexRef $ \segmentIndex ->
    return $! segmentIndex {
      siSchema = Map.insertWith
                 (\_ old -> old)
                 context
                 contextSchema
                 (siSchema segmentIndex)
    }

-- | Removes context (including the index and the schema).
deleteContext :: SegIxRef
              -> Context
              -> IO ()
deleteContext segmentIndexRef context = do
  withSegmentIndex_ segmentIndexRef $ \segmentIndex ->
    return $! segmentIndex {
      siSchema = Map.delete context (siSchema segmentIndex)
    }

-- | Fork a new 'IndexWriter' from a 'SegmentIndex'. This is very cheap
-- and never fails.
newWriter :: SegIxRef
          -> IO IxWrRef
newWriter segmentIndexRef =
  withSegmentIndex segmentIndexRef $ \segmentIndex@SegmentIndex{..} -> do

  indexWriter <- newMVar $! IndexWriter {
      iwIndexDir    = siIndexDir
    , iwNewSegId    = genSegId siSegIdGen
    , iwSchema      = siSchema
    , iwSegments    = siSegments
    , iwNewSegments = SegmentMap.empty
    , iwModSegments = SegmentMap.empty
    , iwSegIxRef    = segmentIndexRef
    }

  let
    segmentIndex' = segmentIndex {
      siSegRefs = SegmentMap.unionWith (+)
                  (SegmentMap.map (\_ -> 1) siSegments)
                  siSegRefs
             }

  return (segmentIndex', indexWriter)

-- | Insert multiple 'ApiDocument's to the index.
insertDocuments :: IxWrRef -> [ApiDocument] -> IO ()
insertDocuments indexWriterRef documents = do
  withIndexWriter_ indexWriterRef $ \indexWriter -> do
    IndexWriter.insertList documents indexWriter

-- | Closes the 'IndexWriter' and commits all changes.
-- After a call to 'closeWriter' any change made is
-- guaranteed to be persisted on disk if there are
-- no write conflicts or exceptions.
closeWriter :: IxWrRef -> IO (Commit ())
closeWriter indexWriterRef = do
  withIndexWriter indexWriterRef $ \indexWriter -> do
    withSegmentIndex (iwSegIxRef indexWriter) $ \segmentIndex -> do
      case IndexWriter.close indexWriter segmentIndex of
        Right segmentIndex' -> do
          -- well, there were no conflicts we are
          -- good to write a new index generation to
          -- disk.
          Store.storeSegmentInfos
            (siIndexDir segmentIndex')
            (siGeneration segmentIndex')
            (Store.segmentIndexToSegmentInfos segmentIndex')

          return ( segmentIndex' { siGeneration =
                                    nextGeneration (siGeneration segmentIndex') }
                 , (indexWriter, Right ())
                 )
        Left conflicts ->
            return (segmentIndex, (indexWriter, Left conflicts))

withSegmentIndex :: SegIxRef -> (SegmentIndex -> IO (SegmentIndex, a)) -> IO a
withSegmentIndex segmentIndexRef action = modifyMVar segmentIndexRef action

withSegmentIndex_ :: SegIxRef -> (SegmentIndex -> IO SegmentIndex) -> IO ()
withSegmentIndex_ segmentIndexRef action = modifyMVar_ segmentIndexRef action

withIndexWriter :: IxWrRef -> (IndexWriter -> IO (IndexWriter, a)) -> IO a
withIndexWriter segmentIndexRef action = modifyMVar segmentIndexRef action

withIndexWriter_ :: IxWrRef -> (IndexWriter -> IO IndexWriter) -> IO ()
withIndexWriter_ indexWriterRef action = modifyMVar_ indexWriterRef action
