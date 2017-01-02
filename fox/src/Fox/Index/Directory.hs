module Fox.Index.Directory where

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
import           Data.Map               (Map)
import qualified Data.Map.Strict        as Map
import           System.Directory

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

posWrite :: Write Position
posWrite = varint

occWrite :: Write (DocId, (Int, Offset))
occWrite = docIdWrite >*< varint >*< varint

writeTermIndex :: IndexDirectory
               -> SegmentId
               -> (FieldName -> Int)
               -> Map Token (Map FieldName Occurrences)
               -> IO ()
writeTermIndex indexDirectory segmentId fieldTy fieldIndex = do
  withAppendFile (termVectorFile indexDirectory) $ \tvFile ->
    withAppendFile (occurrenceFile indexDirectory) $ \occFile ->
    withAppendFile (positionFile indexDirectory) $ \posFile -> do

      let
        tvFlush  = append tvFile
        occFlush = append occFile
        posFlush = append posFile

      withWriteBuffer (32 * 1024) tvFlush $ \tvBuf ->
        withWriteBuffer (32 * 1024) occFlush $ \occBuf ->
        withWriteBuffer (32 * 1024) posFlush $ \posBuf -> do

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
          writeTerm True lastToken token fieldName occurrences = do
            undefined
          writeTerm _ lastToken token fieldName occurrecnes = do
            undefined

        undefined
  where
    write_ :: WriteBuffer -> Write a -> a -> IO ()
    write_ writeBuffer (W size put) a = do
      _ <- write writeBuffer (size a) (put a)
      return ()

termVectorFile :: IndexDirectory -> FilePath
termVectorFile = undefined

occurrenceFile :: IndexDirectory -> FilePath
occurrenceFile = undefined

positionFile :: IndexDirectory -> FilePath
positionFile = undefined
