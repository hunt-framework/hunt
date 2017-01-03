{-# LANGUAGE BangPatterns #-}
module Fox.Index.Directory (
    IndexDirErr(..)
  , IndexDirectory
  , openIndexDirectory
  , writeTermIndex
  ) where

import           Fox.Analyze            (Token)
import           Fox.IO.Buffer          (WriteBuffer, flush, offset,
                                         withWriteBuffer, write)
import           Fox.IO.Files
import           Fox.IO.Write
import           Fox.Types
import qualified Fox.Types.DocIdMap     as DocIdMap
import           Fox.Types.Document
import qualified Fox.Types.Positions    as Positions

import           Control.Monad.Except
import           Data.Bits
import           Data.Foldable
import           Data.Key
import           Data.Map               (Map)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Foreign      as Text
import           System.Directory
import           System.FilePath

newtype IndexDirectory = IndexDirectory { _unIndexDirectory :: FilePath }
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

-- unused due to GHC not being able to optimize big tuple
-- away.
_termWrite :: Write (Int, (Text, (FieldOrd, (Int, Offset))))
_termWrite = varint >*< text >*< varint >*< varint >*< varint
{-# INLINE _termWrite #-}

writeTermIndex :: IndexDirectory
               -> SegmentId
               -> (FieldName -> FieldOrd)
               -> Map Token (Map FieldName Occurrences)
               -> IO ()
writeTermIndex indexDirectory segmentId fieldOrd fieldIndex = do

  -- open all files needed for index writing
  withAppendFile (termVectorFile indexDirectory segmentId) $ \tvFile ->
    withAppendFile (occurrenceFile indexDirectory segmentId) $ \occFile ->
    withAppendFile (positionFile indexDirectory segmentId) $ \posFile -> do

      let
        defaultBufSize :: Int
        defaultBufSize = 32 * 1024

        -- Full buffers are flushed with these
        tvFlush  = append tvFile
        occFlush = append occFile
        posFlush = append posFile

      -- allocate WriteBuffers for any file we want to write.
      -- WriteBuffers keep track of the current offset which
      -- is handy for cross referencing records between files.
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
            write_ occBuf occWrite
              (docId, (Positions.size positions, positionsOffset))


          -- write the delta of a term to buffer. sameToken parameter
          -- is an optimization when we know token is the same as lastToken.
          writeTerm :: Bool -> Token -> Token -> FieldName -> Occurrences -> IO ()
          writeTerm sameToken lastToken token fieldName occurrences = do

            -- start by writing the occurrences to the occs file
            -- remember the offset where we started for this term.
            occOffset <- offset occBuf
            forWithKey_ occurrences $ \docId positions -> writeOccs docId positions

            let
              -- by calculating the common prefix with the last token
              -- we only need to write the suffix of the current token
              -- effectively delta encoding the terms.

              -- if a token occurrs in different fields we know we can
              -- skip the common prefix calculation.
              (prefix, suffix) | sameToken = (lastToken, Text.empty)
                               | otherwise =
                                   case Text.commonPrefixes lastToken token of
                                     Just (p, _, s) -> (p, s)
                                     Nothing        -> (Text.empty, token)

              prefixLen = Text.lengthWord16 prefix `unsafeShiftL` 2
              numOccs   = DocIdMap.size occurrences
              fieldOrd_ = fieldOrd fieldName

            -- we need to split these guys up because GHC is not smart enough
            -- to optimize the deeply nested tuples away.
            write_ tvBuf (varint >*< text)
              (prefixLen, suffix)
            write_ tvBuf (varint >*< varint >*< varint)
              (fieldOrd_, (numOccs, occOffset))


        -- loop over all tokens and fields
        _ <- foldlWithKeyM (\lastToken token fields ->
                              let foldFields notFirst fieldName occs = do
                                    writeTerm notFirst lastToken token fieldName occs
                                    return True
                              in do _ <- foldlWithKeyM foldFields False fields
                                    return token
                           ) Text.empty fieldIndex

        flush tvBuf
        flush occBuf
        flush posBuf

        return ()
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
