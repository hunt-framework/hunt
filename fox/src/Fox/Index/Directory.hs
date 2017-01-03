module Fox.Index.Directory (
    IndexDirErr(..)
  , IndexDirectory
  , openIndexDirectory
  , writeTermIndex
  ) where

import           Fox.Analyze            (Token)
import           Fox.IO.Buffer          (WriteBuffer, offset, withWriteBuffer,
                                         write)
import           Fox.IO.Files
import           Fox.IO.Write
import           Fox.Types
import           Fox.Types.Document
import qualified Fox.Types.Positions    as Positions

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Key
import           Data.Map               (Map)
import qualified Data.Map.Strict        as Map
import           System.Directory
import           System.FilePath

newtype IndexDirectory = IndexDirectory { unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

data IndexDirErr = ErrInvalidDirectory
                 | ErrIndexLocked

openIndexDirectory :: FilePath -> IO (Either IndexDirErr IndexDirectory)
openIndexDirectory indexDir = runExceptT $ do
    -- bail out early so we don't overwrite anything
  -- in case indexDir refers to a file.
  fileExists <- liftIO $ doesFileExist indexDir
  when fileExists $
    throwError ErrInvalidDirectory

  -- check explicitly for existence to avoid
  -- exceptions.
  dirExists <- liftIO $ doesDirectoryExist indexDir
  unless dirExists $
    liftIO $ createDirectoryIfMissing True indexDir

  return $ IndexDirectory indexDir

-- | Position in a file numbered from zero.
type Offset = Int

docIdWrite :: Write DocId
docIdWrite = unDocId >$< varint
{-# INLINE docIdWrite #-}

posWrite :: Write Position
posWrite = varint
{-# INLINE posWrite #-}

occWrite :: Write (DocId, (Int, Offset))
occWrite = docIdWrite >*< varint >*< varint
{-# INLINE occWrite #-}

writeTermIndex :: IndexDirectory
               -> SegmentId
               -> (FieldName -> Int)
               -> Map Token (Map FieldName Occurrences)
               -> IO ()
writeTermIndex indexDirectory segmentId fieldTy fieldIndex = do
  withAppendFile (termVectorFile indexDirectory segmentId) $ \tvFile ->
    withAppendFile (occurrenceFile indexDirectory segmentId) $ \occFile ->
    withAppendFile (positionFile indexDirectory segmentId) $ \posFile -> do

      let
        defaultBufSize :: Int
        defaultBufSize = 32 * 1024

        tvFlush  = append tvFile
        occFlush = append occFile
        posFlush = append posFile

      withWriteBuffer defaultBufSize tvFlush $ \tvBuf ->
        withWriteBuffer defaultBufSize occFlush $ \occBuf ->
        withWriteBuffer defaultBufSize posFlush $ \posBuf -> do

        let
          writePosition :: Position -> IO ()
          writePosition position = do
            write_ posBuf posWrite position

          writeOccs :: DocId -> Positions -> IO ()
          writeOccs docId positions = do
            positionsOffset <- offset posBuf
            for_ (Positions.toAscList positions) writePosition
            write_ occBuf occWrite (docId, (Positions.size positions, positionsOffset))

          writeTerm :: Bool -> Token -> Token -> FieldName -> Occurrences -> IO ()
          writeTerm sameToken lastToken token fieldName occurrences = do
            forWithKey_ occurrences $ \docId positions -> writeOccs docId positions

        forWithKey_ fieldIndex $ \token fields -> do
          forWithKey_ fields $ \fieldName occs -> do
            writeTerm False token token fieldName occs
  where
    write_ :: WriteBuffer -> Write a -> a -> IO ()
    write_ writeBuffer (W size put) a = do
      _ <- write writeBuffer (size a) (put a)
      return ()

termVectorFile :: IndexDirectory -> SegmentId -> FilePath
termVectorFile indexDirectory segmentId =
  indexDirectory <//> show segmentId <.> "tv"

occurrenceFile :: IndexDirectory -> SegmentId -> FilePath
occurrenceFile indexDirectory segmentId =
  indexDirectory <//> show segmentId <.> "occs"

positionFile :: IndexDirectory -> SegmentId -> FilePath
positionFile indexDirectory segmentId =
  indexDirectory <//> show segmentId <.> "pos"

(<//>) :: IndexDirectory -> FilePath -> FilePath
(<//>) (IndexDirectory indexDirectory) path = indexDirectory </> path
