{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex (
    SegIxRef
  , newSegmentIndex
  , insertContext
  , deleteContext

  , newWriter
  , insertDocuments
  , closeWriter

  ) where

import           Hunt.Common.ApiDocument            (ApiDocument)
import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import qualified Hunt.SegmentIndex.Commit           as Commit
import qualified Hunt.SegmentIndex.IndexWriter      as IndexWriter
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap (SegmentMap)
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import qualified System.Directory                   as Directory

-- | Create new 'SegmentIndex' at the given index directory.
newSegmentIndex :: FilePath
                -> IO SegIxRef
newSegmentIndex indexDir = do

  -- TODO: bail out if index directory contains an index
  Directory.createDirectoryIfMissing True indexDir

  segIdGen <- newSegIdGen
  siRef    <- newMVar $! SegmentIndex {
      siGeneration = generationZero
    , siIndexDir   = indexDir
    , siSegIdGen   = segIdGen
    , siSchema     = Map.empty
    , siSegments   = SegmentMap.empty
    , siSegRefs    = SegmentMap.empty
    }
  return siRef

-- | Inserts a 'Context' into the index.
--  /Note/: Does nothing if the context already exists.
insertContext :: SegIxRef
              -> Context
              -> ContextSchema
              -> IO ()
insertContext sixref cx cxs = do
  withSegmentIndex sixref $ \si ->
    let
      si' = si {
          siSchema = Map.insertWith (\_ old -> old) cx cxs (siSchema si)
        }
    in return (si', ())

-- | Removes context (including the index and the schema).
deleteContext :: SegIxRef
              -> Context
              -> IO ()
deleteContext sixref cx = do
  withSegmentIndex sixref $ \si ->
    let
      si' = si {
          siSchema = Map.delete cx (siSchema si)
        }
    in return (si', ())

-- | Fork a new 'IndexWriter' from a 'SegmentIndex'. This is very cheap
-- and never fails.
newWriter :: SegIxRef
          -> IO IxWrRef
newWriter sigRef = withSegmentIndex sigRef $ \si@SegmentIndex{..} -> do

  ixwr <- newMVar $! IndexWriter {
      iwIndexDir    = siIndexDir
    , iwNewSegId    = genSegId siSegIdGen
    , iwSchema      = siSchema
    , iwSegments    = siSegments
    , iwNewSegments = SegmentMap.empty
    , iwModSegments = SegmentMap.empty
    , iwSegIxRef    = sigRef
    }

  let
    si' = si {
      siSegRefs = SegmentMap.unionWith (+)
                  (SegmentMap.map (\_ -> 1) siSegments)
                  siSegRefs
             }

  return (si', ixwr)

-- | Insert multiple 'ApiDocument's to the index.
insertDocuments :: IxWrRef -> [ApiDocument] -> IO ()
insertDocuments ixwrref docs = do
  withIndexWriter ixwrref $ \ixwr -> do
    ixwr' <- IndexWriter.insertList docs ixwr
    return (ixwr', ())

-- | Closes the 'IndexWriter' and commits all changes.
-- After a call to 'closeWriter' any change made is
-- guaranteed to be persisted on disk if there are
-- no write conflicts or exceptions.
closeWriter :: IxWrRef -> IO (CommitResult ())
closeWriter ixwrref = do
  withIndexWriter ixwrref $ \ixwr -> do
    r <- withSegmentIndex (iwSegIxRef ixwr) $ \si -> do

      -- check for conflicts with the 'SegmentIndex'
      case IndexWriter.close ixwr si of
        CommitOk si' -> do

          let
            nextSegId :: SegmentId
            nextSegId = maybe segmentZero fst
                        $ SegmentMap.findMax (siSegments si')

          -- well, there were no conflicts we are
          -- good to write a new index generation to
          -- disk.
          Commit.writeMetaIndex
            (siIndexDir si')
            (siGeneration si')
            nextSegId
            (siSchema si')
            (siSegments si')

          return $ ( si'
                   , CommitOk ()
                   )

        CommitConflicts conflicts ->
          return $ ( si
                   , CommitConflicts conflicts
                   )

    return (ixwr, r)

withSegmentIndex :: SegIxRef -> (SegmentIndex -> IO (SegmentIndex, a)) -> IO a
withSegmentIndex ref action = modifyMVar ref action

withIndexWriter :: IxWrRef -> (IndexWriter -> IO (IndexWriter, a)) -> IO a
withIndexWriter ref action = modifyMVar ref action
