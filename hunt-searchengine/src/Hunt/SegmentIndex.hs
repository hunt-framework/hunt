{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex where

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
  siRef    <- newTMVarIO $! SegmentIndex {
      siGeneration = generationZero
    , siIndexDir   = indexDir
    , siSegIdGen   = segIdGen
    , siSchema     = Map.empty
    , siSegments   = SegmentMap.empty
    , siSegRefs    = SegmentMap.empty
    }
  return siRef

insertContext :: SegIxRef
              -> Context
              -> ContextSchema
              -> IO ()
insertContext sixref cx cxs = do
  atomically $ do
    six <- readTMVar sixref
    putTMVar sixref $! six {
        siSchema = Map.insertWith (\_ old -> old) cx cxs (siSchema six)
      }
  -- TODO: we need to commit  a new index version

deleteContext :: SegIxRef
              -> Context
              -> IO ()
deleteContext sixref cx = do
  atomically $ do
    six <- readTMVar sixref
    putTMVar sixref $! six {
        siSchema = Map.delete cx (siSchema six)
      }
  -- TODO: we need to commit here

-- | Fork a new 'IndexWriter' from a 'SegmentIndex'. This is very cheap
-- and never fails.
newIndexWriter :: SegIxRef
               -> IO IxWrRef
newIndexWriter sigRef = atomically $ do
  si@SegmentIndex{..} <- readTMVar sigRef

  ixwr <- newTMVar $! IndexWriter {
      iwIndexDir    = siIndexDir
    , iwNewSegId    = genSegId siSegIdGen
    , iwSchema      = siSchema
    , iwSegments    = siSegments
    , iwNewSegments = SegmentMap.empty
    , iwModSegments = SegmentMap.empty
    , iwSegIxRef    = sigRef
    }

  putTMVar sigRef $! si {
        siSegRefs = SegmentMap.unionWith (+)
                    (SegmentMap.map (\_ -> 1) siSegments)
                    siSegRefs
      }

  return ixwr

insertDocuments :: IxWrRef -> [ApiDocument] -> IO ()
insertDocuments ixwrref docs = do
  ixwr  <- atomically $ takeTMVar ixwrref
  ixwr' <- IndexWriter.insertList docs ixwr
  atomically $ putTMVar ixwrref $! ixwr'

closeIndexWriter :: IxWrRef -> IO (CommitResult ())
closeIndexWriter ixwrref = do
  -- TODO: make this exception safe!
  ixwr  <- atomically $ readTMVar ixwrref
  segix <- atomically $ readTMVar (iwSegIxRef ixwr)

  case IndexWriter.close ixwr segix of
    CommitOk segix'           -> do

      let
        nextSegId :: SegmentId
        nextSegId = maybe segmentZero fst
                    $ SegmentMap.findMax (siSegments segix)

      Commit.writeMetaIndex
        (siIndexDir segix)
        (siGeneration segix)
        nextSegId
        (siSchema segix)
        (siSegments segix)

      atomically $ putTMVar (iwSegIxRef ixwr) $! segix'
      return $ CommitOk ()
    CommitConflicts conflicts ->
      return $ CommitConflicts conflicts
