module Hunt.ContextIndex.Snapshot.ReadWrite where

import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Snapshot.Files
import           Hunt.ContextIndex.Types
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           System.FilePath
import           Text.Printf

readSnapshots' :: MonadIO m => ContextTypes -> [IxFileSet] -> m [Snapshot]
readSnapshots' cxTypes ixFiles
  = do forM ixFiles $ \ixf -> do
         bytes <- liftIO $ LByteString.readFile (ixfTerms ixf)
         let cm = Get.runGet (getContextMap cxTypes) bytes
         return Snapshot { snId              = ixfSnapshotId ixf
                         , snDeletedDocs     = mempty
                         , snDeletedContexts = mempty
                         , snContextMap      = cm
                         }

readSnapshots :: MonadIO m => ContextTypes -> FilePath -> m [Snapshot]
readSnapshots cxTypes dir
  = do ixfiles <- liftIO $ listSnapshotFiles dir
       readSnapshots' cxTypes ixfiles

writeSnapshot :: MonadIO m => Snapshot -> FilePath -> m ()
writeSnapshot sn ixDir
  = do liftIO $
         LByteString.writeFile (ixDir </> ixFile) (Put.runPut (putContextMap (snContextMap sn)))
  where
    ixFile = printf "snapshot-%.8o.terms" (unSnapshotId (snId sn))

getContextMap :: ContextTypes -> Get ContextMap
getContextMap cxTypes
  = do m <- Ix.gets' impls :: Get [(Context, Ix.IndexImpl)]
       return (mkContextMap (Map.fromDistinctAscList m))
  where
    impls = fmap ctIxImpl cxTypes

putContextMap :: ContextMap -> Put
putContextMap = put . cxMap
